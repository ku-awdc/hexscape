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
#' map <- st_sfc(st_multipolygon(list(list(as.matrix(corners))))) |> st_as_sf()
#' points <- st_sfc(lapply(1:10, function(x) st_point(runif(2,0,10)))) |> st_as_sf()
#' random <- randomise_voronoi(map, points) |> mutate(Index = 1:n())
#' ggplot(random) + geom_sf_label(aes(label=Index)) + geom_sf_label(aes(geometry=RandomPoint, label=Index), col="red")
#'
#'
#' @export
randomise_voronoi <- function(map, points, randomise_size=5L, sample_size=3L, max_tries=3L, verbose=1L){

  ## TODO: add buffer to stop two points being close to each other (as an argument to sample_points)
  ## TODO: change active geometry to RandomPoint in return value (and maybe add the line showing the change, as well as a jitter distance etc?)

  stopifnot(inherits(map, "sf"), inherits(map, "data.frame"))
  stopifnot(inherits(points, "sf"), inherits(points, "data.frame"))

  stopifnot(st_crs(map)==st_crs(points))

  ## Delegate to get simple Voronoi tesselation:
  bbox <- map |> st_union() |> st_bbox()
  bmap <- matrix(bbox[c(1,2,1,4,3,4,3,2,1,2)], ncol=2, byrow=TRUE) |> list() |> list() |> st_multipolygon() |> st_sfc() |> st_as_sf()
  voronoi <- discretise_voronoi(bmap, points) |> mutate(Index = 1:n())

  ## Then use pairwise distance matrices to get the closest S centroids to
  ## each point:
  st_distance(voronoi) |>
    ## Cheat by making the distance to self negative:
    {function(x) x - diag(nrow(points))}() |>
    apply(2, function(x) rank(x, ties.method="random")[1:randomise_size], simplify=FALSE) ->
    closest
  stop("THIS RANK IS BROKEN")
  stopifnot(nrow(points)==length(closest))

  ## And calculate the selection probability based on frequency of appearance
  ## (all should appear at least once):
  freq <- closest |> unlist() |> factor(levels=seq_len(nrow(points))) |> table()
  stopifnot(length(freq) == nrow(points), all(freq)>=1L)

  ## Then

  ## Delegate to get sampled points:
  cat("Getting random points...\n")
  samples <- sample_points(voronoi, size=sample_size, verbose=verbose) |>
    mutate(SampleIndex = 1:n())

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
