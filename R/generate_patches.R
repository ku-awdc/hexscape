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
#'
#' @export
generate_patches <- function(landscape, hex_width, reference_point=st_centroid(landscape), land_use=NULL, add_removed=FALSE, min_prop = 0.01, simplify_keep=0.1){

  st <- Sys.time()

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
    stopifnot(is.factor(land_use$Category), "Impassable" %in% levels(land_use$Category), "Passable" %in% levels(land_use$Category))
    use_categories <- TRUE
  }
  if(inherits(landscape, "sf")) landscape <- landscape[[attr(landscape, "sf_column", TRUE)]]
  stopifnot(inherits(landscape, "sfc"))
  landscape <- st_buffer(st_union(landscape), dist=0)
  bbox <- st_bbox(landscape)

  # TODO: get point directly rather than via bbox:
  refy <- st_bbox(reference_point)["ymin"]
  refx <- st_bbox(reference_point)["xmin"]

  stopifnot(is.numeric(min_prop) && length(min_prop)==1 && min_prop <= 1 && min_prop >= 0)

  ## We will use an axial coordinate system for hexagons a-la:
  # https://www.redblobgames.com/grids/hexagons/#map-storage

  # Row numbers (r) are relatively easy:
  yrange <- as.numeric(refy - st_bbox(landscape)[c("ymin","ymax")]) / ((hexlth+hexhgt)/2)
  # r <- seq(floor(yrange[2]-1), ceiling(yrange[1]+1))
  r_using <- seq(floor(yrange[2]), ceiling(yrange[1]))

  # For column numbers (q) we need to distort the boundary box relative to the reference point
  # i.e. we need the length of the opposite (distortion) based on length of the adjacent and
  # known angle (1/12 of a circle)
  xrange <- (c(
    as.numeric(bbox["xmin"] + (bbox["ymin"] - refy) * tanpi(1/6)),
    as.numeric(bbox["xmax"] + (bbox["ymax"] - refy) * tanpi(1/6))
  ) - refx) / hexwth
  #q <- seq(floor(xrange[1]-1), ceiling(xrange[2]+1))
  q_using <- seq(floor(xrange[1]), ceiling(xrange[2]))

  ## Then generate the centroids of the hexagons we need
  hexagons <- expand_grid(r=r_using, q=q_using) %>%
    mutate(y = refy - r*(hexlth+hexhgt)/2, yy=y) %>%
    mutate(x = refx + r*hexwth/2 + q*hexwth, xx=x) %>%
    # Filter out anything too far away from the lanscape:
    st_as_sf(coords=c("xx","yy")) %>%
    mutate(dist = st_distance(geometry, landscape)[,1]) %>%
    filter(dist < (hexhgt/1.9)) %>%
    as_tibble() %>%
    select(-dist)

  cat("Creating", nrow(hexagons), "hexagons...\n")
  hexagons %>%
    mutate(Index = 1:n()) %>%
    split(.$Index) %>%
    pblapply(function(.x) .x %>% mutate(geometry = genpoly(x, y), centroid = st_sfc(st_point(c(x,y))))) %>%
    bind_rows() ->
  patches

  cat("Intersecting with the provided landscape...\n")
  patches %>%
    st_as_sf() %>%
    mutate(OK = st_intersects(geometry, landscape, sparse=FALSE)[,1]) %>%
    filter(OK) %>%
    mutate(geometry = st_intersection(geometry, landscape)) %>%
    select(Index, r, q, centroid, geometry) %>%
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
    impassable <- st_union(impassable) %>%
      ms_simplify(keep=simplify_keep, keep_shapes=FALSE) %>%
      st_intersection(landscape)
    # ggplot(st_simplify(impassable)) + geom_sf()+ coord_sf(xlim=c(500000, 550000), ylim=c(6100000, 6150000), crs= 25832, datum=sf::st_crs(25832))
    # ggplot(st_simplify(impassable)) + geom_sf()+ coord_sf(xlim=c(520000, 530000), ylim=c(6120000, 6130000), crs= 25832, datum=sf::st_crs(25832))

    pblapply(seq_len(nrow(patches)), function(i){
		ss <- try({
			suppressWarnings(st_cast(st_difference(patches[i,], impassable), to="POLYGON"))
		})
		if(inherits(ss,"try-error")) browser()
      # NB: using st_difference allows categories to have lower resolution than the landscape
    }) ->
    patches

    ## Add a patch with missing index reflecting impassable areas:
    if(add_removed){
      impatch <- tibble(Index=NA_integer_, r=NA_real_, q=NA_real_,
                      centroid=st_centroid(impassable),
                      area = as.numeric(st_area(impassable)),
                      geometry = impassable) %>%
        st_as_sf(sf_column_name = "geometry")

      patches <- c(patches, list(impatch))
    }

  }else{
  ## Otherwise just cast the patches to polygon:
    pblapply(seq_len(nrow(patches)), function(i){
		suppressWarnings(st_cast(st_buffer(patches[i,], 0.0), to="POLYGON"))
    }) ->
    patches
  }

  ## Finish by removing the now obsolete patches and re-indexing:
  cat("Re-indexing hexagons...\n")
  patches %>%
    bind_rows() %>%
#    ms_simplify(keep=simplify_keep, keep_shapes=FALSE) %>%
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

    ## TODO: break up into batches of <=100 just to get a progress bar
    make_chunks <- function(x, max_per_chunk=100L){
      nchunks <- ceiling(nrow(patches)/100L)
      ppc <- rep(floor(length(x) / nchunks), nchunks)
      if(sum(ppc) < length(x)){
        ppc[1:(length(x)-sum(ppc))] <- ppc[1:(length(x)-sum(ppc))] + 1
      }
      stopifnot(sum(ppc) == length(x))
      ppc_st <- cumsum(lag(ppc,1,0))
      ppc_en <- ppc_st + ppc
      chunks <- lapply(seq(1L,nchunks,1L), function(y) seq(ppc_st[y]+1,ppc_en[y],1))
      ccheck <- unlist(chunks)
      stopifnot(length(x) == length(ccheck) && all(x %in% ccheck))
      return(chunks)
    }

    cat("Intersecting land use with the landscape...\n")

    levels(land_use$Category) %>%
      str_subset(fixed("Impassable"), negate=TRUE) %>%
      `names<-`(.,.) %>%
      as.list() %>%
      pblapply(function(category){
        comb <- land_use %>%
          filter(Category == category) %>%
          st_union() %>%
          st_intersection(landscape) %>%
          ## Sometimes we get points - remove these:
          st_collection_extract("POLYGON") %>%
          ## TODO: purrr::quietly
          st_union() %>%
          ms_simplify(keep=simplify_keep, keep_shapes=TRUE, explode=TRUE, method="dp") %>%
          ## st_buffer with dist=0 resolves any self intersections:
          st_buffer(dist=0) %>%
          st_union()
      }) ->
      relevant_land

    # ggplot(relevant_land[[2]]) + geom_sf()+ coord_sf(xlim=c(500000, 550000), ylim=c(6100000, 6150000), crs= 25832, datum=sf::st_crs(25832))

    cat("Determining land use summaries...\n")

    browser()

    make_chunks(which(!is.na(patches$Index))) %>%
      pblapply(function(ch){
        # Note: geometry column is actually a list, so that is not copied here:
        tptch <- patches[ch,]
        names(relevant_land) %>%
          lapply(function(rl){
            tptch %>%
              mutate(ok = st_intersects(geometry, relevant_land[[rl]], sparse=FALSE)) %>%
              filter(ok) %>%
              mutate(Category = rl, area = as.numeric(st_area(st_intersection(geometry, relevant_land[[rl]])))) %>%
              as_tibble() %>%
              select(Index, Category, area)
          }) %>%
          bind_rows()
    }) %>%
      bind_rows() %>%
      ## Add back in patches that have been removed:
      full_join(
        patches %>%
          as_tibble() %>%
          filter(!is.na(Index)) %>%
          select(Index, total_area=area) %>%
          expand_grid(Category = names(relevant_land)),
        by=c("Index","Category")
      ) %>%
      replace_na(list(area = 0.0)) ->
    patch_land_use

    stopifnot(all(na.omit(patches$Index) %in% patch_land_use$Index), nrow(patch_land_use) == (nrow(patches)-1)*length(relevant_land))

    ## Some land area is lost due to simplification:
    patch_land_use %>%
      group_by(Index, total_area) %>%
      summarise(areasum = sum(area), .groups="drop") %>%
      mutate(loss = 1- areasum/total_area) %>%
      arrange(desc(loss)) ->
      patchsum

    ## TODO: make sure not too many patches are excluded - maybe look at bbox for landscape and land use??

    if(FALSE){

    head(patchsum)
    tail(patchsum)

    #with(patchsum, plot(total_area, areasum)); abline(0,1)
    #mean(patchsum$loss)

    patches %>% filter(Index=="157")
    ggplot() +
      geom_sf(data=patches %>% filter(row %in% 6:8, col %in% 47:49)) +
      geom_sf(data=patches %>% filter(Index=="157"), fill="red", col="red")

    patches %>% filter(Index=="482")
    ggplot() +
      geom_sf(data=patches %>% filter(row %in% 13:15, col %in% 48:50)) +
      geom_sf(data=patches %>% filter(Index=="482"), fill="red", col="red")

    }

    patch_land_use %>%
      group_by(Index) %>%
      mutate(area_sum = sum(area), proportion = area / area_sum) %>%
      ungroup() %>%
      mutate(Category = factor(Category, levels=levels(land_use$Category))) %>%
      arrange(Category) %>%
      select(Index, area_sum, Category, proportion) %>%
      spread(Category, proportion, fill=0.0) %>%
      bind_rows(
        tibble(Index = NA_integer_, area_sum = NA_real_, Category = names(relevant_land), proportion = NA_real_) %>%
          spread(Category, proportion)
      ) ->
      patch_land_use_wd
    stopifnot(all(names(patch_land_use_wd)[1:2] == c("Index","area_sum")))
    names(patch_land_use_wd) <- c("Index","area_sum",str_c("LU_", names(patch_land_use_wd)[-(1:2)]))

    stopifnot(all(patches$Index %in% patch_land_use_wd$Index), nrow(patch_land_use_wd) == nrow(patches))

    patches <- patches %>%
      full_join(patch_land_use_wd, by="Index")

    ## Where areasum is zero attribute it all to passable:
    patches %>%
      mutate(LU_Passable = case_when(
        area_sum < sqrt(.Machine$double.eps) ~ 1.0 - (LU_Low + LU_Medium + LU_High),
        TRUE ~ LU_Passable
      )) ->
      patches

    ## Make sure proportions sum to unity:
    checksum <- patches %>%
      as_tibble() %>%
      filter(!is.na(Index)) %>%
      select(starts_with("LU")) %>%
      as.matrix() %>%
      apply(1,sum)

    # TODO: remove browser here
    if(!isTRUE(all.equal(rep(1.0,length(checksum)), checksum))) browser()
    stopifnot(all.equal(rep(1.0,length(checksum)), checksum))

    if(FALSE){
    ggplot(patches, aes(fill=LU_Passable)) + geom_sf()
    ggplot(patches, aes(fill=LU_Low)) + geom_sf()
    ggplot(patches, aes(fill=LU_Medium)) + geom_sf()
    ggplot(patches, aes(fill=LU_High)) + geom_sf()
    ggplot(patches, aes(fill=LU_Medium+LU_High)) + geom_sf()
    ggplot(patches, aes(fill=LU_Passable+LU_Low)) + geom_sf()

    summary(patches)

    plot(ecdf(patches$LU_Passable))
    }
  }else{
	  ## Add area_sum for equivalence with other code:
	  patches <- patches %>% mutate(area_sum = area)
  }

  ## Finally re-calculate the centroids:
  cat("Recalculating centroids...\n")
  patches %>%
    mutate(hex_centroid = centroid, centroid = st_centroid(geometry)) %>%
    select(Index, r, q, centroid, hex_centroid, area, lu_sum=area_sum, starts_with("LU"), geometry) ->
    patches

  st_crs(patches$centroid) <- st_crs(patches$geometry)
  st_crs(patches$hex_centroid) <- st_crs(patches$geometry)

  class(patches) <- c("patches", class(patches))
  attr(patches, "hex_width") <- hex_width
  attr(patches, "min_prop") <- min_prop

  cat("Done in ", round(as.numeric(Sys.time() - st, units="mins")), " mins\n", sep="")

  warning("ADD REFERENCE POINT AS ATTR")

  return(patches)

  ## TODO: check that bbox of (st_union of) land_use is bigger than the landscape, and return % unexplained landuse??
  ## TODO: transfer code from here down to separate function

  # Some patches are split e.g. 45,4 contains an island and part of the mainland:
  # patches %>% group_by(row,col) %>% mutate(N=n()) %>% filter(N>1)
  # ggplot(patches %>% filter(row %in% 3:5, col %in% 44:46), aes(label=str_c(col,",",row))) + geom_sf() + geom_sf_label()
  # Some are split into more than 2:
  # patches %>% group_by(row,col) %>% mutate(N=n()) %>% filter(N>2)
  # ggplot(patches %>% filter(row %in% 45:47, col %in% 46:48), aes(label=str_c(col,",",row))) + geom_sf() + geom_sf_label()


}
