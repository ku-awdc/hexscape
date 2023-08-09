#' Title
#'
#' @param map
#' @param size
#' @param sample_scale
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
sample_points <- function(map, size=1L, sample_scale=100L, verbose=1L){

  ## TODO: add buffer to stop two points being close to each other (as an argument to sample_points)

  stopifnot(inherits(map, "sf"), inherits(map, "data.frame"), "Index" %in% names(map))
  stopifnot(all(table(map[["Index"]])==1L))

  ## Sample points within the map provided by iterative rejection sampling:
  all_points <- vector('list', length=sample_scale^2L)
  i <- 1L
  totals <- numeric(nrow(map))
  bbox_using <- map |> st_bbox()

  if(verbose > 0L) pb <- txtProgressBar(style=3)
  while(any(totals < size)){
    if(i > length(all_points)){
      length(all_points) <- length(all_points)*2L
    }
    bbox_using |>
      st_as_sfc(crs=st_crs(map)) |>
      st_sample(size=size*nrow(map)*sample_scale, by_polygon=FALSE) ->
    new_pts

    tibble(point = new_pts) |>
      mutate(match = st_intersects(point, map, sparse=TRUE) |> as.numeric()) |>
      filter(!is.na(match), match %in% which(totals < size)) ->
    keep_pts

    newtot <- keep_pts |> count(match)
    totals[newtot[["match"]]] <- totals[newtot[["match"]]] + newtot[["n"]]
    bbox_using <- map[totals < size,] |> st_bbox()

    all_points[[i]] <- keep_pts
    i <- i + 1L

    if(verbose > 0L) setTxtProgressBar(pb, sum(totals >= size) / nrow(map))
  }
  if(verbose > 0L) close(pb)

  all_points |>
    bind_rows() |>
    group_by(match) |>
    slice(1:size) |>
    ungroup() |>
    mutate(Index = map[["Index"]][match]) ->
  rv

  stopifnot(all(table(rv[["match"]])==size), nrow(rv)==(nrow(map)*size))

  map |>
    as.data.frame() |>
    select(Index) |>
    full_join(rv |> select(Index, geometry=point), by="Index") |>
    st_as_sf(crs=st_crs(map)) ->
  rv

  return(rv)

}
