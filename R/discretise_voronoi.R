#' Title
#'
#' @param map
#' @param points
#'
#' @return
#' @export
#'
#' @importFrom sf st_voronoi st_collection_extract st_contains st_geometry
#'
#' @examples
discretise_voronoi <- function(map, points){

  stopifnot(inherits(map, "sf"), inherits(map, "data.frame"))
  stopifnot(inherits(points, "sf"), inherits(points, "data.frame"))

  stopifnot(st_crs(map)==st_crs(points))
  mapsf <- st_union(map)

  ## First get the Voronoi tesselation:
  points |>
    st_union() |>
    st_voronoi(envelope = st_as_sfc(st_bbox(mapsf), crs=st_crs(mapsf))) |>
    st_collection_extract() ->
    voronois

  ## Then reorder to match input points:
  vt_index <- as.numeric(st_intersects(points, voronois, sparse=TRUE))
  if(length(vt_index) != nrow(points)){
    stop("An unexpected error occured when generating Voronoi tesselations")
  }

  ## Then re-order the Voronoi cells, merge into data frame, and intersect etc:
  points |>
    ungroup() |>
    mutate(geometry = voronois[vt_index]) |>
    mutate(geometry = st_intersection(geometry, mapsf) |> st_make_valid()) |>
    mutate(centroid = st_centroid(geometry, of_largest_polygon=FALSE)) |>
    mutate(Area = st_area(geometry) |> set_units(km^2) |> as.numeric()) |>
    relocate(Area, centroid, geometry, .after = last_col()) ->
    rv

  st_geometry(rv) <- "geometry"

  # ggplot(tt[1:10,]) + geom_sf(aes(geometry=geometry)) + geom_sf(aes(geometry=centroid), col="red") + geom_sf()

  return(rv)

}
