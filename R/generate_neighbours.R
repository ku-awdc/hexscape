#' Generate a sparse matrix of neighbours for patches (i.e. edges in the graph)
#'
#' @param patches an object created by \code{\link{generate_patches}}
#' @param calculate_border should the length of the common border to neighbours be calculated?
#' @param buffer_dist a tolerance limit defining which borders are considered to be contiguous
#'
#' @export
generate_neighbours <- function(patches, calculate_border=TRUE, buffer_dist=0.001*hex_width){

  st <- Sys.time()

  stopifnot(inherits(patches, "patches"))

  ## Remove the fake index (impassable areas):
  patches <- patches %>% filter(!is.na(Index))

  hex_width <- attr(patches, "hex_width", TRUE)
  hexwth <- hex_width
  # Height (corner to corner):
  hexhgt <- 2*hexwth / 3^0.5
  # Side length:
  hexlth <- hexhgt/2
  # Max area:
  hexarea <- sqrt(3)*hexwth^2/2
  min_prop <- attr(patches, "min_prop", TRUE)

  stopifnot(is.numeric(buffer_dist) && length(buffer_dist)==1 && buffer_dist > 0)

  ## Work out nearest neighbours based on hexagon properties:

  cat("Calculating neighbours for", nrow(patches), "hexagons...\n")

  patches %>%
    mutate(geometry = st_buffer(geometry, dist = buffer_dist)) %>%
    select(Index, hex_centroid, area, geometry) ->
    neighbours

  if (!calculate_border) {
    ## Neighbours method 1:  use st_join to see which patches and neighbours overlap:
    neighbours %>%
      select(Neighbour = Index,
             nb_centroid = hex_centroid,
             nb_area = area) %>%
      st_join(neighbours, join = st_intersects) %>%
      filter(Neighbour != Index) %>%
      mutate(Border = NA_real_) ->
      neighbours
    # Note: this is fast for small # patches but may not scale as well?
    # and (more importantly) does not give us the length of the border

  } else{ # if !calculate_border

    ## Neighbours method 2:  use st_intersection to get actual area (and therefore length) of border:
    patches %>%
      mutate(geometry = st_buffer(geometry, dist = buffer_dist)) ->
      neighbours

    ## Neighbours are limited to one of the following 8 options:
    expand_grid(r_adj=seq(-1,1,1), q_adj=seq(-1,1,1)) %>%
      # Filter out the same patch:
      filter(r_adj!=0 | q_adj!=0) %>%
      # Then filter out the q-1 and r-1, and q+1 and r+1:
      filter(! (r_adj+q_adj) %in% c(-2, 2)) %>%
      mutate(NeighbourNumber = 1:n()) %>%
      split(.$NeighbourNumber) %>%
      map_df( ~ bind_cols(.x, neighbours)) %>%
      mutate(r = r + r_adj, q = q + q_adj) %>%
      select(Neighbour=Index, r, q, nb_centroid=hex_centroid, nb_geometry=geometry, nb_area=area) %>%
      as_tibble() %>%
      right_join(neighbours, by=c("r","q")) %>%
      select(Index, Neighbour, area, hex_centroid, geometry, nb_area, nb_centroid, nb_geometry) %>%
      arrange(Index, Neighbour) ->
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
          x <- neighbours_incomplete_area[i,]
          intsct <- st_area(st_intersection(x$geometry, x$nb_geometry))
          return(intsct)

          ### DEMO CODE:

          # Adjust for the buffer distance:
          intsct <- st_intersection(x$geometry, x$nb_geometry)
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
#      mutate(Border = (as.numeric(Intsct, units="m") - (buffer_dist*2)) / (buffer_dist*2)) %>%
      mutate(Border = (as.numeric(Intsct, units = "m")) / (buffer_dist * 2)) %>%
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

  cat("Calculating direction to these neighbours...\n")

  # Finally add the direction to these neighbours:
  ## TODO: this may be more efficient based on q and r:
  st_coordinates(neighbours$nb_centroid - neighbours$hex_centroid) %>%
    as_tibble() %>%
    bind_cols(neighbours %>% select(Index, Neighbour, Border, nb_area)) %>%
    mutate(Direction = case_when(
      abs(Y) < sqrt(.Machine$double.eps) & X > 0 ~ "E",
      abs(Y) < sqrt(.Machine$double.eps) & X < 0 ~ "W",
      Y > 0 & X > 0 ~ "NE",
      Y < 0 & X > 0 ~ "SE",
      Y > 0 & X < 0 ~ "NW",
      Y < 0 & X < 0 ~ "SW"
    )) %>%
    mutate(Direction = factor(Direction,
                              levels = c("NE", "E", "SE", "SW", "W", "NW"))) %>%
    select(Index, Neighbour, Border, Direction, nb_area) ->
    neighbours

  if(FALSE){
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
  }

  class(neighbours) <- c("neighbours", class(neighbours))

  cat("Done in ", round(as.numeric(Sys.time() - st, units="mins")), " mins\n", sep="")

  return(neighbours)
}
