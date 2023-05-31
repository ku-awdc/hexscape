#' Generate pairwise connectedness between patches using a numerical approximation to integrating over distances between areas within two patches
#'
#' @param patches a data frame containing polygons and indexes, such as that created by \code{\link{generate_patches}}
#' @param connectedness_fun a vectorised function mapping distances to connectedness i.e. a spatial kernal (TODO: may also take a second argument giving the bearing)
#' @param max_distance the maximum distance beyond which connectedness is assumed to be negligable - default is to determine this automatically
#' @param grid_resolution the resolution of the numerical approximation, in terms of the number of points to evaluate betweeen zero distance and max_distance
#' @param sparse should a sparse data frame be returned, or a dense matrix?
#' @param centroid_distance should additional information of connectedness based on centroids be returned (ignored if !isTRUE(sparse))
#' @param verbose verbosity level (0 is silent, 2 is noisy)
#'
#' @export
generate_connectedness <- function(patches, connectedness_fun, max_distance=NULL, grid_resolution=50, sparse=TRUE, centroid_distance=TRUE, verbose=1L){

  if(FALSE){
    library("sf")
    library("pbapply")
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
    farms <- tibble(Index = 1:100L) |> mutate(geometry = st_sample(landscape, n()) |> st_sfc()) |> st_as_sf()
    patches <- discretise_voronoi(landscape, farms)

    connectedness_fun <- function(x) 0.5 * 1/x
    max_distance=5; grid_resolution=50; verbose=1L
  }

  ## TODO: checks
  grid_resolution <- as.integer(grid_resolution)

  if(!is.data.frame(patches) || !all(c("Index","geometry") %in% names(patches))){
    stop("The specified argument to patches must be a data frame with Index and geometry columns")
  }
  if(any(patches |> count(Index) |> pull(n) > 1L)) stop("Duplicated Index number detected")
  if(any(is.na(patches[["Index"]]))) stop("Non-finite Index number detected")
  patches <- patches |> arrange(Index)
  all_indexes <- patches[["Index"]]

  if(verbose > 1L) cat("Calculating pairwise distances between patches...\n")

  ## Get pairwise distances between patches (based on closest parts of each polygon, i.e. NOT centroid-centroid distances):
  distances <- st_distance(patches)

  ## Get the maximum distance for this landscape:
  bbox <- st_bbox(st_union(patches))
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
  grid_by <- max_distance / grid_resolution
  patches |>
    mutate(Area = st_area(geometry) |> as.numeric()) |>
    mutate(Points = Area / (grid_by*grid_by)) |>
    pull(Points) |>
    round() ->
    grid_total_points
  if(any(grid_total_points == 0L)) stop("One or more patches with zero points")
  ## TODO: make the points on pre-specified x and y coords so that grid_total_points is consistent
  ## TODO: expand C++ class so it does more

  ## Pre-calculate connectedness for a regular grid with height/width = 2*max_distance:
  expand_grid(Col = seq(-grid_resolution, grid_resolution, by=1L),
              Row = seq(-grid_resolution, grid_resolution, by=1L)
              ) |>
    mutate(x = Row * grid_by, y = Col * grid_by) |>
    st_as_sf(coords = c("x","y"), crs = st_crs(patches)) |>
    mutate(Distance = st_point(c(0,0)) |> st_sfc(crs = st_crs(patches)) |> st_distance(geometry) |> as.numeric()) |>
    ## TODO: bearing
    mutate(Connectedness = case_when(
      Col == 0L & Row == 0L ~ NA_real_,
      Distance > max_distance ~ 0.0,
      TRUE ~ connectedness_fun(Distance)
    )) ->
    grid_points

  # ggplot(grid_points, aes(col=log10(Connectedness+1))) + geom_sf()

  grid_matrix <- matrix(grid_points[["Connectedness"]], ncol=(2L*grid_resolution+1L), nrow=(2L*grid_resolution+1L))
  conn_fun <- hexscape:::RcppConnectedness$new(grid_resolution, grid_matrix)

  ## Approximation function in R/C++:
  approxfun <- function(i){

    ## Helper function - TODO: C++ code
    addindex <- function(x, candidates){
      st_contains_properly(candidates, x, sparse=FALSE) |>
        apply(2L, function(y){
          y <- which(y)-1L
          if(length(y)==0L) return(-1L)
          stopifnot(length(y)==1L)
          y
        }) ->
        ii
      x |> mutate(Which = ii)
    }

    ## Find the patches within max_distance:
    candidates <- patches |> slice(which(distances[i,] <= max_distance))

    ## Define a bbox around the target patch and calculate central x and y points:
    bbox <- patches |> slice(i) |> st_bbox()
    stopifnot(!is.na(bbox))
    xpoints_central <- seq(bbox[["xmin"]]+grid_by/2, bbox[["xmax"]], by=grid_by)
    ypoints_central <- seq(bbox[["ymin"]]+grid_by/2, bbox[["ymax"]], by=grid_by)

    ## Calculate all x and y points using a buffer around the central points:
    xpoints <- c(
      xpoints_central[1L] - seq(grid_resolution, 1, by=-1)*grid_by,
      xpoints_central,
      xpoints_central[length(xpoints_central)] + seq(1, grid_resolution, by=1)*grid_by
    )
    stopifnot(abs((xpoints-lag(xpoints))[-1L] - grid_by) < sqrt(.Machine$double.eps))
    ypoints <- c(
      ypoints_central[1L] - seq(grid_resolution, 1, by=-1)*grid_by,
      ypoints_central,
      ypoints_central[length(ypoints_central)] + seq(1, grid_resolution, by=1)*grid_by
    )
    stopifnot(abs((ypoints-lag(ypoints))[-1L] - grid_by) < sqrt(.Machine$double.eps))

    ## Distribute points over this area and add the corresponding Index:
    expand_grid(x = xpoints, y = ypoints) |>
      st_as_sf(coords = c("x","y")) |>
      addindex(candidates) ->
      points
    stopifnot(all(!is.na(points[["Which"]])))
    stopifnot(nrow(points)==(length(xpoints)*length(ypoints)))

    points |> as_tibble() |> count(Which) |>
      arrange(Which) |> filter(Which>=0L) |>
      full_join(
        tibble(Which = (1:nrow(candidates))-1L),
        by="Which"
      ) |>
      replace_na(list(n = 0L)) |> pull(n) ->
      points_within
    stopifnot(length(points_within)==nrow(candidates))

    points_outside <- pmax(0L, grid_total_points[candidates[["Index"]]] - points_within)

    ## Get connectedness:
    target <- which(patches[["Index"]][i] == candidates[["Index"]])
    conn_fun$get_connectedness(length(ypoints_central), length(xpoints_central),
                               target-1L, grid_resolution, points_outside,
                               matrix(points[["Which"]], nrow=length(ypoints), ncol=length(xpoints))
                               ) ->
      connectedness

    ## Calculate mean connectedness to this target patch by source patch and return:
    tibble(
      Source = all_indexes[i],
      Target = candidates[["Index"]],
      Connectedness = connectedness
    )
  }

  ####################
  ## Approximation function in R (slow):
  approxfun_R <- function(i){

    addindex <- function(x, candidates){
      st_contains_properly(patches |> slice(candidates), x, sparse=FALSE) |>
        apply(2L, function(y){
          y <- candidates[which(y)]
          if(length(y)==0L) return(NA_integer_)
          stopifnot(length(y)==1L)
          y
        }) ->
        ii
      x |> mutate(Index = ii) |> filter(!is.na(Index))
    }

    ## Find the patches within max_distance:
    candidates <- which(distances[i,] <= max_distance)
    ## Get a bbox:
    tlscp <- patches |> slice(candidates) |> st_union()
    bbox <- st_bbox(tlscp)

    ## Distribute points over this area and add the corresponding Index:
    expand_grid(x = seq(bbox[["xmin"]], bbox[["xmax"]], by=grid_by), y = seq(bbox[["ymin"]], bbox[["ymax"]], by=grid_by)) |>
      st_as_sf(coords = c("x","y")) |>
      addindex(candidates) ->
      points

    ## Separate into the target and source points:
    target <- points |> filter(Index == i)
    stopifnot(nrow(target)>0L)
    source <- points

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
  ####################


  ## Get output:
  seq_len(nrow(patches)) |>
    pblapply(approxfun) |>
    bind_rows() |>
    ## Correction for density of grid:
    mutate(Connectedness = Connectedness*grid_by*grid_by) ->
    rv

  if(sparse) return(rv)

  up <- unique(patches[["Index"]])

  rv |>
    full_join(
      expand_grid(Source = up, Target = up),
      by=c("Source","Target")
    ) |>
    replace_na(list(Connectedness = 0.0)) |>
    arrange(Source, Target) |>
    pull(Connectedness) |>
    matrix(nrow = length(up), ncol = length(up)) ->
    rvd

  # diag(rvd)
  # plot(rvd, t(rvd))

  return(rvd)

  system.time(output <- approxfun_R(1))
  system.time(output <- approxfun(1))

  ## Note:  this is currently WAAAAY to slow to be useful for reasonable grid_resolution, but there are some inefficiencies we can address:
  # MAJOR:  st_distance is not the fastest way to calculate distance as the points are regular
  #         we can instead frame the target point as 0,0 and use pre-calculated connectedness in a matrix around that
  # MAJOR:  loops are inevitable -> use Rcpp (generating points and overlapping Index is not the bottleneck)
  # MINOR:  Distances between patches are symmetric, so here are calculated twice needlessly

  ## TODO:  also return connectedness within a patch, as we will need to control for this with infection models for full equivalence??
  ## TODO:  implement sparse and centroid_distance arguments

}
