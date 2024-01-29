#' Randomise locations of points based on Voronoi tesselation
#'
#' @param points the points to randomise
#' @param map a landscape to use for masking
#' @param randomise_size the pool size for randomisation
#' @param from_type the basis on which to determine distance between "from" and "to" points: one of voronoi, point or centroid
#' @param to_type the basis on which to determine distance between "from" and "to" points: one of voronoi, point or centroid
#' @param mask_landscape should the Voronoi cells be masked using the map before distances are calculated? Has no effect for from_type=to_type="point"
#' @param additional_info option to provide additional columns in the dataframe returned
#' @param sample_size the number of random points to generate within each Voronoi cell
#' @param max_tries the maximum number of tries before failure
#' @param verbose a positive integer determining how much progress output should be displayed (0=silent)
#'
#' @return The return value depends on the input points:  if the input is an sfc object and additional_info==FALSE, then the output is also an sfc object; if the input is an sf data frame, then the output is also an sf data frame with the random points in the active geometry (which will have the same name as the input); the output is always an sf data frame if additional_info==TRUE, with the randomised points as the active geometry (this will be named geometry if the input is an sfc object, or have the same name as the active geometry in the input if this was an sf data frame)
#' @examples
#' xrange <- c(0, 10)
#' yrange <- c(0, 10)
#' corners <- tribble(~x, ~y,
#'                    xrange[1], yrange[1],
#'                    xrange[2], yrange[1],
#'                    xrange[2], yrange[2],
#'                    xrange[1], yrange[2],
#'                    xrange[1], yrange[1]
#' )
#' library("sf")
#' map <- st_sfc(st_multipolygon(list(list(as.matrix(corners)))))
#' points <- st_sfc(lapply(1:10, function(x) st_point(runif(2,0,10))))
#' random <- randomise_voronoi(points, map, additional_info=TRUE)
#' ggplot(random) +
#'   geom_sf(aes(geometry=VoronoiCell)) +
#'   geom_sf(aes(geometry=VoronoiMasked), alpha=0.5) +
#'   geom_sf(aes(geometry=RandomShift)) +
#'   geom_sf(aes(geometry=RandomPoint)) +
#'   theme_void()
#'
#'
#' @export
randomise_voronoi <- function(points, map, randomise_size=5L, from_type = "point", to_type = "centroid", mask_landscape = FALSE, additional_info = FALSE, sample_size=3L, max_tries=3L, verbose=1L){

  ## TODO: add buffer to stop two points being close to each other (as an argument to sample_points)

  ## TODO: document arguments - points can be a vector or a d.f. - if the latter then the active geo is taken and over-written;  map can be a vector or d.f. - if the latter then the active geo is used;  additional_info creates a data frame if input wasn't one but check column names don't clash
  ## TODO: look for voronoi cells that do not appear in the list of candidates more than once - give a warning or include as output #cells candidate and #cells chosen
  ## TODO: add some random swaps at the end to alleviate the island problem?

  ## Argument checking
  if(inherits(map, "sf")){
    ## An sf data frame:
    # attr(map, "sf_column")
    map <- st_geometry(map) |> st_union()
  }else if(inherits(map, "sfc")){
    map <- st_union(map)
  }else{
    stop("The provided map should either be an sfc object or an sf object with an active geometry")
  }
  stopifnot("The provided map is not an sfc polygon"=inherits(map, c("sfc_POLYGON", "sfc_MULTIPOLYGON")))

  if(inherits(points, "sf")){
    points_is_df <- TRUE
    if(additional_info){
      stopifnot(all(!c("RandomShift","FromType","ToType","VoronoiMasked","VoronoiUnmasked","ExternalCandidates") %in% names(points)))
    }
    pointscolname <- attr(points, "sf_column")
    stopifnot(!is.null(pointscolname), !is.na(pointscolname))
  }else if(inherits(points, "sfc")){
    points_is_df <- FALSE
    points <- tibble(geometry = pointscol) |> st_as_sf(crs=st_crs(points))
    pointscolname <- "geometry"
  }else{
    stop("The provided points should either be an sfc object or an sf object with an active geometry")
  }
  stopifnot(
    "The provided points are not sfc points"=inherits(pointscol, c("sfc_POINT")),
    "The provided points are of length 1"=nrow(points)>1L,
    "The CRS of the map and points don't match"=st_crs(map)==st_crs(points)
  )
  original_points <- st_geometry(points)

  ## Check arguments for types and masking:
  check_type <- function(x){
    type_options <- c("voronoi","point","centroid")
    if(!is.character(x) || length(x)!=1L || is.na(x)) stop(paste0("Invalid ", substitute(x), ": use one of the following: ", paste(type_options, collapse=", ")), call.=FALSE)
    rv <- type_options[pmatch(tolower(x), type_options)]
    if(is.na(rv)) stop(paste0("Unrecognised ", substitute(x), ": use one of the following: ", paste(type_options, collapse=", ")), call.=FALSE)
    rv
  }
  to_type <- check_type(to_type)
  from_type <- check_type(from_type)
  stopifnot(is.logical(mask_landscape), length(mask_landscape)==1L, !is.na(mask_landscape))
  stopifnot(is.logical(additional_info), length(additional_info)==1L, !is.na(additional_info))

  ## Delegate to get simple Voronoi tesselation:
  bbox <- map |> st_bbox()
  bmap <- matrix(bbox[c(1,2,1,4,3,4,3,2,1,2)], ncol=2, byrow=TRUE) |> list() |> list() |> st_multipolygon() |> st_sfc() |> st_as_sf(crs = st_crs(map))
  voronoi <- discretise_voronoi(bmap, points) |> mutate(Index = 1:n())

  ## If masking then do so now (otherwise later):
  voronoi_unmasked <- voronoi
  if(mask_landscape){
    voronoi |>
      mutate(geometry = st_intersection(geometry, map)) |>
      mutate(centroid = st_centroid(geometry)) ->
      voronoi
  }

  ## Then pick the to and from types:
  if(from_type=="voronoi"){
    dist_from <- voronoi
  }else if(from_type=="point"){
    dist_from <- points
  }else if(from_type=="centroid"){
    dist_from <- voronoi
    st_geometry(dist_from) <- "centroid"
  }else{
    stop("Missing from_type")
  }
  if(to_type=="voronoi"){
    dist_to <- voronoi
  }else if(to_type=="point"){
    dist_to <- points
  }else if(to_type=="centroid"){
    dist_to <- voronoi
    st_geometry(dist_to) <- "centroid"
  }else{
    stop("Missing to_type")
  }


  ## Then use pairwise distance matrices to get the closest S centroids to
  ## each point:
  st_distance(dist_from, dist_to) |>
    set_units("m") |>
    ## Cheat by making the distance to self negative:
    {function(x) x - (diag(nrow(points)) |> set_units("m"))}() |>
    apply(2, function(x) which(rank(x, ties.method="random") <= randomise_size), simplify=FALSE) ->
    closest
  stopifnot(nrow(points)==length(closest))

  ## Calculate the number of times a voronoi cell is chosen by a different cell:
  seq_len(nrow(points)) |>
    lapply(function(i){
      c <- closest[[i]]
      c[c!=i]
    }) |>
    do.call("c", args=_) |>
    factor(levels=seq_len(nrow(points))) |>
    table() ->
    chosen_by_other
  stopifnot(names(chosen_by_other)==seq_len(nrow(points)))
  if(any(chosen_by_other == 0L)){
    if(additional_info){
      warning("One or more point had zero external candidate cells (Island problem)")
    }else{
      warning("One or more point had zero external candidate cells (Island problem) - you can re-run with additional_info=TRUE to see which")
    }
  }

  ## And calculate the selection probability based on frequency of appearance
  ## (all should appear at least once):
  freq <- closest |> unlist() |> factor(levels=seq_len(nrow(points))) |> table() |> as.numeric()
  stopifnot(length(freq) == nrow(points), all(freq>=1L), sum(freq)==(nrow(points)*randomise_size))

  ## Then do an st_intersection of the voronoi cells with the provided map (if not already done):
  if(!mask_landscape){
    voronoi |>
      mutate(geometry = st_intersection(geometry, map)) ->
      voronoi
  }

  ## Delegate to get sampled points:
  if(verbose>0L) cat("Getting random points...\n")
  sample_points(voronoi, size=sample_size, verbose=verbose) |>
    mutate(SampleIndex = 1:n()) |>
    full_join(
      tibble(Index = seq_len(nrow(points)), SampleProb = 1/freq),
      by="Index"
    ) ->
    samples

  ## And then sample a point from one of the corresponding Voronoi cells:
  if(verbose>0L) cat("Running reassortment...\n")
  for(rep in seq_len(max_tries)){
    used <- numeric(length(closest))
    #allchosen <- vector('list', length=length(closest))

    if(verbose>0L) pb <- txtProgressBar(style=3)
    for(i in seq_along(closest)){
      samples |>
        filter(Index %in% closest[[i]], !SampleIndex %in% used) |>
        slice_sample(n=1L, weight_by=SampleProb) ->
        chosen

      ## Detect failed algorithm to restart:
      if(nrow(chosen)==0L) break
      stopifnot(nrow(chosen)==1L)
      #allchosen[[i]] <- chosen

      points[[pointscolname]][i] <- chosen[["geometry"]]
      used[i] <- chosen[["SampleIndex"]]

      if(verbose>0L) setTxtProgressBar(pb, i/length(closest))
    }
    if(verbose>0L) close(pb)

    if(!any(used==0L)) break
    if(verbose>0L) cat("Algorithm failed, trying again...\n")
  }
  #allchosen |> bind_rows() |> count(Index)
  if(any(used==0L)) stop("Algorithm failed more than the specified maximum number of tries - you could try re-running the function")

  if(additional_info){
    points[["RandomShift"]] = st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, original_points, points[[pointscolname]], SIMPLIFY=FALSE), crs = st_crs(points))
    stopifnot(nrow(points)==nrow(voronoi))
    points[["FromType"]] <- st_geometry(dist_from)
    points[["ToType"]] <- st_geometry(dist_to)
    points[["VoronoiMasked"]] <- voronoi[["geometry"]]
    points[["VoronoiUnmasked"]] <- voronoi_unmasked[["geometry"]]
    points[["ExternalCandidates"]] <- as.integer(chosen_by_other)
  }

  st_geometry(points) <- pointscolname

  if(!additional_info && !points_is_df){
    points <- st_geometry(points)
  }

  return(points)
}
