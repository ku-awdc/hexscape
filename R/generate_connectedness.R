#' Generate pairwise connectedness between patches using a numerical approximation to integrating over distances between areas within two patches
#'
#' @param patches a data frame containing polygons and indexes, such as that created by \code{\link{generate_patches}}
#' @param connectedness_fun a vectorised function mapping distances to connectedness i.e. a spatial kernal
#' @param max_distance the maximum distance beyond which connectedness is assumed to be negligable - default is to determine this automatically
#' @param grid_resolution the resolution of the numerical approximation, in terms of the number of points to evaluate betweeen zero distance and max_distance
#' @param sparse should a sparse data frame be returned, or a dense matrix?
#' @param extra_info should additional information of connectedness based on centroids be returned (ignored if !isTRUE(sparse))
#' @param verbose verbosity level (0 is silent, 2 is noisy)
#'
#' @export
generate_connectedness <- function(patches, connectedness_fun, max_distance=NULL, grid_resolution=1e3, sparse=TRUE, extra_info=TRUE, verbose=2L){

  if(FALSE){
    library("sf")
    xrange <- c(0, 50)
    yrange <- c(0, 50)
    corners <- tribble(~x, ~y,
                       xrange[1], yrange[1],
                       xrange[2], yrange[1],
                       xrange[2], yrange[2],
                       xrange[1], yrange[2],
                       xrange[1], yrange[1]
    )
    landscape <- st_multipolygon(list(list(as.matrix(corners)))) |> st_sfc() |> st_as_sf()
    farms <- tibble(Index = st_sample(landscape, 100L) |> st_sfc()) |> st_as_sf()
    patches <- discretise_voronoi(landscape, farms)

    connectedness_fun <- function(x) 0.1 * 1/x
    max_distance=5; grid_resolution=20; verbose=2L
  }


  if(!is.data.frame(patches) || !all(c("Index","geometry") %in% names(patches))){
    stop("The specified argument to patches must be a data frame with Index and geometry columns")
  }
  if(any(patches |> count(Index) |> pull(n) > 1L)) stop("Duplicated Index number detected")
  if(any(is.na(patches[["Index"]]))) stop("Non-finite Index number detected")
  all_indexes <- patches[["Index"]]

  if(verbose > 1L) cat("Calculating pairwise distances between patches...\n")

  ## Get pairwise distances between patches (based on closest parts of each polygon, i.e. NOT centroid-centroid distances):
  distances <- st_distance(patches$geometry)

  ## Get the maximum distance for this landscape:
  bbox <- st_bbox(st_union(patches$geometry))
  max_dist_landscape <- st_distance(st_point(bbox[c("xmin","ymin")]), st_point(bbox[c("xmax","ymax")])) |> as.numeric()

  ## If max_distance is not specified, attempt to guess at a reasonable number...
  ## NOTE: this may do badly with different shaped kernals (only tested with inverse distance type)
  if(is.null(max_distance)){
    stop("Algorithm to find max_distance is broken - supply it manually!")

    if(verbose > 0L) cat("Attempting to find an appropriate max_distance ... ")
    mindist <- min(distances[distances > 0])/grid_resolution
    maxdist <- max_dist_landscape*grid_resolution
    full_integral <- integrate(connectedness_fun, lower=mindist, upper=maxdist)$value
    max_distance <- optimise(function(x){
      newint <- integrate(connectedness_fun, lower=mindist, upper=x)$value
      abs(newint-(full_integral*1e-4))
    }, c(mindist, maxdist), maximum = FALSE)$objective
    if(verbose > 0L) cat("value of ", max_distance, " chosen\n\t(WARNING: this might very well NOT be a good choice!)\n", sep="")
    # dd <- seq(1,max_distance*1.5,length.out=grid_resolution)
    # plot(dd, connectedness_fun(dd), type='l')
  }

  ## Now we can do the numerical approximation:

  # Helper function - EXTREMELY inefficient - TODO: C++ code
  addindex <- function(x){
    st_contains_properly(patches |> slice(candidates), x, sparse=FALSE) |>
      apply(2L, function(y){
        y <- which(y)
        if(length(y)==0L) return(NA_integer_)
        stopifnot(length(y)==1L)
        y
      }) ->
      ii
    x |> mutate(Index = ii) |> filter(!is.na(Index))
  }

  grid_by <- max_distance / grid_resolution

  ## Approximation function:
  approxfun <- function(i){

    ## Find the patches within max_distance:
    candidates <- which(distances[i,] <= max_distance)
    ## Get a bbox:
    tlscp <- patches |> slice(candidates) |> st_union()
    bbox <- st_bbox(tlscp)

    ## Distribute points over this area and add the corresponding Index:
    expand_grid(x = seq(bbox[["xmin"]], bbox[["xmax"]], by=grid_by), y = seq(bbox[["ymin"]], bbox[["ymax"]], by=grid_by)) |>
      st_as_sf(coords = c("x","y")) |>
      addindex() ->
      points

    ## Separate into the target and source points:
    target <- points |> filter(Index == i)
    source <- points |> filter(Index != i)

    ## Get distances between every target and source point:
    if(FALSE){
      ## Very inefficient:
      pblapply(seq_len(nrow(target)), function(tt){
        source |>
          mutate(Distance = st_distance(geometry, target[tt,])) |>
          mutate(Connectedness = connectedness_fun(Distance)) |>
          group_by(Index) |>
          summarise(Sum = sum(Connectedness), N = n(), .groups="drop")
      }) |>
        bind_rows() |>
        group_by(Index) |>
        summarise(Connectedness = sum(Sum) / sum(N), .groups="drop")
    }

    ## Calculate mean connectedness to this target patch by source patch and return:
    source |>
      mutate(Connectedness = st_distance(source, target) |> connectedness_fun() |> apply(1, mean)) |>
      as_tibble() |>
      group_by(Index) |>
      summarise(Connectedness = mean(Connectedness), .groups="drop") |>
      mutate(Destination = all_indexes[i], Source = all_indexes[Index]) |>
      select(Source, Destination, Connectedness)
  }

  ## Get output:
  seq_len(nrow(patches)) |>
    pblapply(approxfun) |>
    bind_rows() ->
    rv

  return(rv)

  system.time(output <- approxfun(1))

  ## Note: this is currently WAAAAY to slow to be useful for reasonable grid_resolution, but there are some inefficiencies we can address:
  # MAJOR:  st_distance is not the fastest way to calculate distance as the points are regular
  #         we can instead frame the target point as 0,0 and use pre-calculated connectedness in a matrix around that
  # MAJOR:  loops are inevitable -> use Rcpp (generating points and overlapping Index is not the bottleneck)
  # MINOR:  Distances between patches are symmetric, so here are calculated twice needlessly

}
