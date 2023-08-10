#' Title
#'
#' @param points
#' @param map
#' @param size
#' @param buffer
#' @param verbose
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
#' landscape <- st_sfc(st_multipolygon(list(list(as.matrix(corners))))) |> st_as_sf()
#' points <- st_sfc(lapply(1:10, function(x) st_point(runif(2,0,10)))) |> st_as_sf()
#' random <- randomise_voronoi(landscape, points) |> mutate(Index = 1:n())
#' ggplot(random) + geom_sf_label(aes(label=Index)) + geom_sf_label(aes(geometry=RandomPoint, label=Index), col="red")
#'
#'
#' @export
randomise_voronoi <- function(map, points, randomise_size=5L, sample_size=10L, max_tries=3L, verbose=1L){

  ## TODO: add buffer to stop two points being close to each other (as an argument to sample_points)
  ## TODO: change active geometry to RandomPoint in return value (and maybe add the line showing the change, as well as a jitter distance etc?)


  stopifnot(inherits(map, "sf"), inherits(map, "data.frame"))
  stopifnot(inherits(points, "sf"), inherits(points, "data.frame"))

  stopifnot(st_crs(map)==st_crs(points))
  mapsf <- st_union(map)

  ## Delegate to get Voronoi tesselation:
  voronoi <- discretise_voronoi(map, points) |> mutate(Index = 1:n())

  ## Delegate to get sampled points:
  cat("Getting random points...\n")
  samples <- sample_points(voronoi, size=sample_size, verbose=verbose) |>
    mutate(SampleIndex = 1:n())

  ## Then use pairwise distance matrices to get the closest S centroids to
  ## each point:
  st_distance(voronoi, voronoi$centroid) |>
    apply(2, function(x) order(x)[1:randomise_size], simplify=FALSE) ->
    closest
  stopifnot(nrow(voronoi)==length(closest))

  ## And then sample a point from one of the corresponding Voronois
  if(verbose>0L) cat("Running reassortment...\n")
  for(rep in seq_len(max_tries)){
    used <- numeric(length(closest))

    if(verbose>0L) pb <- txtProgressBar(style=3)
    for(i in seq_along(closest)){
      samples |>
        filter(Index %in% closest[[i]], !SampleIndex %in% used) |>
        slice_sample(n=1L) ->
        chosen

      ## Detect failed algorithm to restart:
      if(nrow(chosen)==0L) break
      stopifnot(nrow(chosen)==1L)

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
  if(any(used==0L)) stop("Algorithm failed more than the specified maximum number of tries - you could try re-running the function")

  return(points)
}