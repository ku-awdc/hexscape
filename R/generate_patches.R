#' Title
#'
#' @param landscape
#' @param hex_width
#' @param calculate_border
#' @param buffer_dist
#' @param min_prop
#'
#' @import sf
#' @importFrom rmapshaper ms_simplify
#'
#' @examples
#'
#' xrange <- c(0, 50)
#' yrange <- c(0, 50)
#' corners <- tribble(~x, ~y,
#'     xrange[1], yrange[1],
#'     xrange[2], yrange[1],
#'     xrange[2], yrange[2],
#'     xrange[1], yrange[2],
#'     xrange[1], yrange[1]
#' )
#' landscape <- sf::st_sfc(sf::st_multipolygon(list(list(as.matrix(corners)))))

#' ## And add our hexagons:
#' patches <- generate_patches(landscape, hex_width=2)
#' patches
#' ggplot(patches, aes(label=Index)) + geom_sf() + geom_sf_text()
#' patches <- generate_patches(landscape, hex_width=2, calculate_border = TRUE)
#' patches
#' ggplot(patches, aes(label=Index)) + geom_sf() + geom_sf_text()
#'
#' @export
generate_patches <- function(landscape, hex_width, land_use=NULL, calculate_border=FALSE, buffer_dist=0.001*hex_width, min_prop = 0.01, simplify_keep=0.5){

  stopifnot(is.numeric(hex_width) && length(hex_width)==1 && !is.na(hex_width))
  hexwth <- hex_width

  # Height (corner to corner):
  hexhgt <- 2*hexwth / 3^0.5
  # Side length:
  hexlth <- hexhgt/2
  # Max area:
  hexarea <- sqrt(3)*hexwth^2/2

  # Matrix of point pairs for centroid 0,0:
  genpoly <- function(x, y){
    hexpoints <- t(matrix(c(
      0, hexhgt/2,
      hexwth/2, hexlth/2,
      hexwth/2, -hexlth/2,
      0, -hexhgt/2,
      -hexwth/2, -hexlth/2,
      -hexwth/2, hexlth/2,
      0, hexhgt/2
    ), nrow=2) + c(x,y))
    st_sfc(st_polygon(list(hexpoints)), crs=st_crs(landscape))
  }

  cat("Processing landscape input...\n")
  use_categories <- FALSE
  if(!is.null(land_use)){
    stopifnot(inherits(land_use, "data.frame"))
    stopifnot("Category" %in% names(land_use))
    stopifnot(is.factor(land_use$Category), length(levels(land_use$Category))==4, all(c("Impassable","Passable","Food","Habitat") %in% levels(land_use$Category)))
    use_categories <- TRUE
  }
  if(inherits(landscape, "sf")) landscape <- landscape[[attr(landscape, "sf_column", TRUE)]]
  stopifnot(inherits(landscape, "sfc"))
  landscape <- st_union(landscape)
  bbox <- st_bbox(landscape)

  stopifnot(is.numeric(buffer_dist) && length(buffer_dist)==1 && buffer_dist > 0)
  stopifnot(is.numeric(min_prop) && length(min_prop)==1 && min_prop <= 1 && min_prop >= 0)

  # An over-estimate of the number of centroids needed:
  nx <- ceiling((bbox["xmax"]-bbox["xmin"]) / hexwth) +1
  ny <- ceiling((bbox["ymax"]-bbox["ymin"]) / (hexhgt + hexlth)) *2  +1

  cat("Creating", ny*nx, "hexagons...\n")
  expand_grid(row = seq(0,ny-1,by=1), col = seq(0,nx-1,by=1)) %>%
    mutate(offset = as.logical(row %% 2 < 0.5)) %>%
    mutate(x = bbox["xmin"] + case_when( !offset ~ hexwth*col, offset ~ hexwth/2 + hexwth*col)) %>%
    mutate(y = bbox["ymin"] + (hexhgt+hexlth)*row/2) %>%
    mutate(Index = 1:n()) %>%
    split(.$Index) %>%
    map_df( ~ .x %>% mutate(geometry = genpoly(x, y), centroid = st_sfc(st_point(c(x,y))))) %>%
    st_as_sf() %>%
    mutate(OK = st_intersects(geometry, landscape, sparse=FALSE)[,1]) %>%
    filter(OK) %>%
    mutate(geometry = st_intersection(geometry, landscape)) %>%
    select(Index, row, col, centroid, geometry) %>%
    # We need to remove small ones here as otherwise some can't be cast to POLYGON below:
    mutate(area = as.numeric(st_area(geometry), units="m")) %>%
    filter(area >= min_prop * hexarea) ->
  patches

  ## Split hexagons that are made discontinuous by the landscape and remove small ones:

  ## If we have categories to worry about then subtract impassable:
  cat("Casting the hexagons to polygons...\n")
  if(use_categories && any(land_use$Category=="Impassable")){
    impassable <- land_use %>%
      filter(Category == "Impassable")
    impassable <- impassable[[attr(impassable, "sf_column", TRUE)]]
    impassable <- st_intersection(landscape, st_union(impassable))

    pblapply(seq_len(nrow(patches)), function(i){
      suppressWarnings(st_cast(st_difference(patches[i,], impassable), to="POLYGON"))
      # NB: using st_difference allows categories to have lower resolution than the landscape
    }) ->
    patches

    ## Add a patch with missing index reflecting impassable areas:
    impatch <- tibble(Index=NA_integer_, row=NA_real_, col=NA_real_,
                      centroid=st_centroid(impassable),
                      area = as.numeric(st_area(impassable)),
                      geometry = impassable) %>%
      st_as_sf(sf_column_name = "geometry")

    patches <- c(patches, list(impatch))

  }else{
  ## Otherwise just cast the patches to polygon:
    pblapply(seq_len(nrow(patches)), function(i){
      suppressWarnings(st_cast(patches[i,], to="POLYGON"))
    }) ->
    patches
  }

  ## Finish by simplifying the patches slightly, removing the now obsolete patches
  ## and re-indexing:
  patches %>%
    bind_rows() %>%
    ms_simplify(keep=simplify_keep, keep_shapes=FALSE) %>%
    ## st_buffer with dist=0 resolves any self intersections:
    mutate(geometry = st_buffer(geometry, dist=0)) %>%
    # See also:
    # https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
    mutate(area = as.numeric(st_area(geometry), units="m")) %>%
    filter(area >= min_prop * hexarea) %>%
    # Information is duplicated by combination of row and col:
    # mutate(HexIndex = Index, Index = 1:n()) ->
    arrange(is.na(Index)) %>%
    mutate(Index = case_when(is.na(Index) ~ NA_integer_, TRUE ~ 1:n())) ->
  patches

  # ggplot(patches, aes(fill=row%%2==1)) + geom_sf()
  # ggplot(patches, aes(fill=is.na(Index))) + geom_sf()

  ## Then loop over the Category types to get areas for each patch:
  if(use_categories){
    cat("Determining land use summaries...\n")

    pblapply(unique(land_use$Category), function(category){

      if(category=="Impassable") return(NULL)

      relevant_land <- land_use %>%
        filter(Category == category)
      relevant_land <- relevant_land[[attr(relevant_land, "sf_column", TRUE)]]
      relevant_land <- st_intersection(landscape, st_union(relevant_land))

      area <- patches %>%
        mutate(ok = st_intersects(geometry, relevant_land, sparse=FALSE)) %>%
        filter(ok) %>%
        mutate(Category = category, total_area=area, area = as.numeric(st_area(st_intersection(geometry, relevant_land)))) %>%
        as_tibble() %>%
        select(Index, total_area, Category, area)

      return(area)
    }) %>%
      bind_rows() %>%
      select(Index, total_area, Category, area) ->
    patch_land_use

    ## Some land area is lost due to simplification:
    patch_land_use %>%
      group_by(Index, total_area) %>%
      summarise(areasum = sum(area), .groups="drop") %>%
      mutate(loss = 1- areasum/total_area) %>%
      arrange(desc(loss)) ->
      patchsum
    head(patchsum)
    tail(patchsum)

    patches %>% filter(Index=="157")
    ggplot() +
      geom_sf(data=patches %>% filter(row %in% 6:8, col %in% 47:49)) +
      geom_sf(data=patches %>% filter(Index=="157"), fill="red", col="red")

    patches %>% filter(Index=="482")
    ggplot() +
      geom_sf(data=patches %>% filter(row %in% 13:15, col %in% 48:50)) +
      geom_sf(data=patches %>% filter(Index=="482"), fill="red", col="red")

    patch_land_use %>%
      group_by(Index) %>%
      mutate(proportion = area / sum(area)) %>%
      select(Index, Category, proportion) %>%
      spread(Category, proportion, fill=0) %>%
      select(Index, Food, Habitat) ->
      patch_land_use_wd

    patches <- patches %>%
      full_join(patch_land_use_wd, by="Index") %>%
      replace_na(list(Food=0, Habitat=0)) %>%
      mutate(Food = case_when(is.na(Index) ~ NA_real_, TRUE ~ Food)) %>%
      mutate(Habitat = case_when(is.na(Index) ~ NA_real_, TRUE ~ Habitat)) %>%
      mutate(BoarScore = Food/2 + Habitat)

  }

  return(patches)

  ## TODO: check that bbox of (st_union of) land_use is bigger than the landscape, and return % unexplained landuse??
  ## TODO: transfer code from here down to separate function

  # Some patches are split e.g. 45,4 contains an island and part of the mainland:
  # patches %>% group_by(row,col) %>% mutate(N=n()) %>% filter(N>1)
  # ggplot(patches %>% filter(row %in% 3:5, col %in% 44:46), aes(label=str_c(col,",",row))) + geom_sf() + geom_sf_label()
  # Some are split into more than 2:
  # patches %>% group_by(row,col) %>% mutate(N=n()) %>% filter(N>2)
  # ggplot(patches %>% filter(row %in% 45:47, col %in% 46:48), aes(label=str_c(col,",",row))) + geom_sf() + geom_sf_label()

  ## Work out nearest neighbours based on hexagon properties:

  cat("Calculating neighbours for", nrow(patches), "hexagons...\n")

  patches %>%
    mutate(geometry = st_buffer(geometry, dist=buffer_dist)) %>%
    select(Index, centroid, area, geometry) ->
    neighbours

  if(!calculate_border){

    ## Neighbours method 1:  use st_join to see which patches and neighbours overlap:
    neighbours %>%
      select(Neighbour = Index, nb_centroid=centroid, nb_area=area) %>%
      st_join(neighbours, join=st_intersects) %>%
      filter(Neighbour != Index) %>%
      mutate(Border = NA_real_) ->
      neighbours
    # Note: this is fast for small # patches but may not scale as well?
    # and (more importantly) does not give us the length of the border

  }else{ # if !calculate_border

    ## Neighbours method 2:  use st_intersection to get actual area (and therefore length) of border:
    patches %>%
      mutate(geometry = st_buffer(geometry, dist=buffer_dist)) ->
      neighbours

    ## Neighbours are limited to one of the following 8 options:
    expand_grid(row_adj=seq(-1,1,1), col_adj=seq(-1,1,1)) %>%
      filter(row_adj!=0 | col_adj!=0) %>%
      mutate(NeighbourNumber = 1:n()) %>%
      split(.$NeighbourNumber) %>%
      map_df( ~ bind_cols(.x, neighbours)) %>%
      mutate(row = row + row_adj, col = col + col_adj) %>%
      # Remove the 2 non-neighbours based on odd vs even row:
      mutate(offset = as.logical(row %% 2 < 0.5)) %>%
      mutate(using = case_when(
        row_adj == 0 ~ TRUE,
        col_adj == 0 ~ TRUE,
        !offset & col_adj>0 ~ TRUE,
        offset & col_adj<0 ~ TRUE,
        TRUE ~ FALSE
      )) %>%
      filter(using) %>%
      select(Neighbour=Index, row, col, nb_centroid=centroid, nb_geometry=geometry, nb_area=area) %>%
      as_tibble() %>%
      right_join(neighbours, by=c("row","col")) %>%
      select(Index, Neighbour, area, centroid, geometry, nb_area, nb_centroid, nb_geometry) ->
      neighbours

    #	rdpt <- patches %>% slice_sample(n=1)
    #	ggplot() +
    #	  geom_sf(data=rdpt, fill="red") +
    #	  geom_sf(aes(geometry=nb_geometry), neighbours %>% filter(Index==rdpt$Index))

    # We have mostly 6 potential neighbours, but sometimes fewer (coastlines),
    # and sometimes more (split patches)
    # neighbours %>% count(Index) %>% count(n)
    # NB: some patches may even have zero potential neighbours (but unlikely)

    ## Now we have to calculate intersections a fairly slow way:
    neighbours %>%
      mutate(complete_area = area > ((1-min_prop)*hexarea) & nb_area > ((1-min_prop)*hexarea)) ->
      neighbours

    ## Shortcut for neighbours we know to be complete (i.e. the majority for simple landscapes):
    neighbours %>%
      filter(complete_area) %>%
      mutate(Border = hexlth) ->
      neighbours_complete_area

    ## No shortcut for the rest:
    neighbours %>%
      filter(!complete_area) ->
      neighbours_incomplete_area

    neighbours_incomplete_area$Intsct <- pbsapply(seq_len(nrow(neighbours_incomplete_area)),
                                                  function(i){
                                                    x <<- neighbours_incomplete_area[i,]
                                                    intsct <- st_area(st_intersection(x$geometry, x$nb_geometry))
                                                    return(intsct)

                                                    ### DEMO CODE:

                                                    # Adjust for the buffer distance:
                                                    area <- as.numeric(st_area(intsct), units="m")
                                                    bdr <- (area - (buffer_dist*2)) / (buffer_dist*2)

                                                    lstr <- st_union(st_intersection(x$boundary, x$nb_boundary))
                                                    # Not always true!!!
                                                    stopifnot(st_geometry_type(lstr)=="LINESTRING")
                                                    as.numeric(st_length(lstr), units="m")

                                                    ggplot() +
                                                      geom_sf(data=patches %>% filter(Index %in% c(x$Index, x$Neighbour))) +
                                                      geom_sf(data=intsct, col="red") +
                                                      geom_sf(data=lstr, col="blue")

                                                  })

    neighbours_incomplete_area %>%
      mutate(Border = (as.numeric(Intsct, units="m") - (buffer_dist*2)) / (buffer_dist*2)) %>%
      filter(!is.na(Border)) %>%
      select(-Intsct) %>%
      bind_rows(neighbours_complete_area) %>%
      filter(Border > min_prop*hexlth) ->
      neighbours

    # We still have mostly 6 neighbours, but sometimes fewer (coastlines),
    # and more rarely more (split patches that aren't islands)
    # neighbours %>% count(Index) %>% count(n)
    # NB: some patches may even have zero potential neighbours (but unlikely)

  } # \if calculate_border

  # Finally add the direction to these neighbours:
  st_coordinates(neighbours$nb_centroid - neighbours$centroid) %>%
    as_tibble() %>%
    bind_cols(neighbours %>% select(Index, Neighbour, Border, nb_area)) %>%
    mutate(Direction = case_when(
      abs(Y)<sqrt(.Machine$double.eps) & X > 0 ~ "E",
      abs(Y)<sqrt(.Machine$double.eps) & X < 0 ~ "W",
      Y > 0 & X > 0 ~ "NE",
      Y < 0 & X > 0 ~ "SE",
      Y > 0 & X < 0 ~ "NW",
      Y < 0 & X < 0 ~ "SW"
    )) %>%
    mutate(Direction = factor(Direction, levels=c("NE","E","SE","SW","W","NW"))) %>%
    select(Index, Neighbour, Border, Direction, nb_area) ->
    neighbours

  ## Note: we can easily add "most significant" directional neighbours using:
  msnbs <- neighbours %>%
    group_by(Index, Direction) %>%
    arrange(desc(Border), desc(nb_area)) %>%
    slice(1) %>%
    ungroup() %>%
    select(Index, Neighbour, Direction) %>%
    spread(Direction, Neighbour)
  ## BUT if not calculating borders then the "true" directional neighbour is chosen
  ## according to the neighbour patch size - i.e. may not be correct!!

  # For now I will do this for back-compatibility BUT I am going to change the class to remove this
  # when I put it into a package (ie patch-level info will be separate to the network graph)
  patches <- full_join(patches, msnbs, by="Index")

  ## Finally re-calculate the centroids:
  patches %>%
    mutate(hex_centroid = centroid, centroid = st_centroid(geometry)) %>%
    select(Index, row, col, centroid, hex_centroid, area, geometry, NE:NW) ->
    patches

  stopifnot(all(levels(neighbours$Direction) %in% names(patches)))

  class(patches) <- c("patches", class(patches))
  return(patches)

}
