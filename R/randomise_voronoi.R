#' Randomise locations of points based on Voronoi tesselation
#'
#' @param points the points to randomise
#' @param map a landscape to use for masking
#' @param randomise_size the pool size for randomisation
#' @param from_type the basis on which to determine distance between "from" and "to" points: one of voronoi, point or centroid
#' @param to_type the basis on which to determine distance between "from" and "to" points: one of voronoi, point or centroid
#' @param mask_landscape should the Voronoi cells be masked using the map before distances are calculated? Has no effect for from_type=to_type="point"
#' @param ektra_info option to provide additional columns in the dataframe returned
#' @param sample_size the number of random points to generate within each Voronoi cell
#' @param max_tries the maximum number of tries before failure
#' @param verbose a positive integer determining how much progress output should be displayed (0=silent)
#'
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
#' map <- st_sfc(st_multipolygon(list(list(as.matrix(corners))))) |> st_as_sf()
#' points <- st_sfc(lapply(1:10, function(x) st_point(runif(2,0,10)))) |> st_as_sf()
#' random <- randomise_voronoi(map, points, additional_info=TRUE) |> mutate(Index = 1:n())
#' ggplot(random) +
#'   geom_sf(aes(geometry=VoronoiCell)) +
#'   geom_sf(aes(geometry=VoronoiMasked), alpha=0.5) +
#'   geom_sf(aes(geometry=RandomShift)) +
#'   geom_sf(aes(geometry=RandomPoint)) +
#'   theme_void()
#'
#'
#' @export
randomise_voronoi <- function(map, points, randomise_size=5L, from_type = "point", to_type = "voronoi", mask_landscape = FALSE, additional_info = FALSE, sample_size=3L, max_tries=3L, verbose=1L){

  ## TODO: add buffer to stop two points being close to each other (as an argument to sample_points)

  stopifnot(inherits(map, "sf"), inherits(map, "data.frame"))
  stopifnot(inherits(points, "sf"), inherits(points, "data.frame"))

  stopifnot(st_crs(map)==st_crs(points))

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
  bbox <- map |> st_union() |> st_bbox()
  bmap <- matrix(bbox[c(1,2,1,4,3,4,3,2,1,2)], ncol=2, byrow=TRUE) |> list() |> list() |> st_multipolygon() |> st_sfc() |> st_as_sf(crs = st_crs(map))
  voronoi <- discretise_voronoi(bmap, points) |> mutate(Index = 1:n())

  ## If masking then do so now (otherwise later):
  voronoi_unmasked <- voronoi
  if(mask_landscape){
    mapsf <- map |> st_union()
    voronoi |>
      mutate(geometry = st_intersection(geometry, mapsf)) |>
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

  ## And calculate the selection probability based on frequency of appearance
  ## (all should appear at least once):
  freq <- closest |> unlist() |> factor(levels=seq_len(nrow(points))) |> table() |> as.numeric()
  stopifnot(length(freq) == nrow(points), all(freq>=1L), sum(freq)==(nrow(points)*randomise_size))

  ## Then do an st_intersection of the voronoi cells with the provided map (if not already done):
  if(!mask_landscape){
    mapsf <- map |> st_union()
    voronoi |>
      mutate(geometry = st_intersection(geometry, mapsf)) ->
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

      if(i==1L){
        ## Set up input data frame for output:
        points <- points |> mutate(RandomPoint = chosen[["geometry"]])
      }else{
        points[["RandomPoint"]][i] <- chosen[["geometry"]]
      }
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
    points[["RandomShift"]] = st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, st_geometry(points), points[["RandomPoint"]], SIMPLIFY=FALSE), crs = st_crs(points))
    stopifnot(nrow(points)==nrow(voronoi))
    points[["FromType"]] <- st_geometry(dist_from)
    points[["ToType"]] <- st_geometry(dist_to)
    points[["VoronoiMasked"]] <- voronoi[["geometry"]]
    points[["VoronoiUnmasked"]] <- voronoi_unmasked[["geometry"]]
  }

  st_geometry(points) <- "RandomPoint"

  return(points)
}
