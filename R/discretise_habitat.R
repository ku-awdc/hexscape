#' Title
#'
#' @param habitat an sf data frame with active geometry column and Density column
#' @param max_size maximum (approximate) size of output patches
#' @param min_size minumum (approxiamte) size of output patches
#' @param patch_density assumed density of output patches in terms of carrying capacity per km^2
#' @param raster_size size of hexagons/squares used for rasterisation
#' @param raster_shape one of hexagons or squares
#' @param h_adj adjustment of the bandwidth used by [MASS::kde2d()]
#' @param point_density density of points used for kde2d and k-means clustering
#' @param verbose verbocity setting (0 = silent)
#'
#' @return
#' @export
#'
#' @importFrom units set_units as_units
#' @importFrom sf st_geometry
#' @import checkmate
#'
#' @examples
discretise_habitat <- function(habitat, max_size=as_units(5, "km^2"),
                               min_size=as_units(0.5, "km^2"),
                               patch_density = as_units(1.0, "1/km^2"),
                               raster_size = as_units(0.05, "km^2"),
                               raster_shape = c("hexagons","squares"),
                               h_adj=0.5, point_density=as_units(100, "1/km^2"),
                               verbose=2L){

  # TODO: argument checks
  ## The habitat argument should be an sf data frame with column giving "Density"
  ## max_size is in km2 (and is not actually max; it is possible for some to be larger)
  ## min_size is in km2 (and may not actually be min, although most likely)
  ## raster_size is in km2 - this is effectively fed to kde2d as well
  ## raster_shape is hexagon or square
  ## h_adj is the adjustment for the default h for kde2d
  ## point_density is the density of points fed into kde2d (for Density==1)
  ## verbose is either a logical or integerish

  ## TODO: check to see if these are already units, and assume km2 if not
  units(max_size) <- "km^2"
  units(min_size) <- "km^2"
  units(patch_density) <- "1/km^2"
  units(raster_size) <- "km^2"
  units(point_density) <- "1/km^2"

  stopifnot(min_size >= as_units(0, "km^2"), max_size >= 2*min_size)
  stopifnot(raster_size >= as_units(0, "km^2"), raster_size < max_size)
  stopifnot(point_density > as_units(0, "1/km^2"))

  if(verbose >= 2L){
    applyfun <- pblapply
  }else{
    applyfun <- lapply
  }

  raster_shape <- match.arg(raster_shape)

  stopifnot(is.data.frame(habitat), inherits(habitat, "sf"),
            !is.null(st_geometry(habitat)), "Density" %in% names(habitat))

  habitat <- ungroup(habitat)
  habitat[["geometry"]] <- st_geometry(habitat)

  # Calculate total carrying capacity:
  if(verbose > 1L) cat("Calculating total carrying capacity...\n")
  habitat |>
    mutate(Total = st_area(.data$geometry) * .data$Density) |>
    as_tibble() |>
    summarise(Total = sum(.data$Total), .groups="Drop") |>
    pull(.data$Total) |>
    set_units("km^2") |>
    as.numeric() ->
    total_capacity
  total_size <- total_capacity / patch_density

  # Then add artificial points representing habitat suitability at given density:
  if(verbose > 0L) cat("Generating artificial points...\n")

  habitat |>
    filter(.data$Density > 0.0) |>
    group_by(.data$Density) |>
    summarise(geometry = st_union(.data$geometry), .groups="drop") |>
    rowwise() |>
    group_split() |>
    applyfun(function(x){
      bb <- st_bbox(x)
      ## Convert point_density in km2 to x/y spacing of points in m:
      by <- 1/sqrt(x[["Density"]] * as.numeric(set_units(point_density, "1/m^2")))
      expand_grid(x=seq(bb["xmin"], bb["xmax"], by=by), y=seq(bb["ymin"], bb["ymax"], by=by)) |>
        st_as_sf(coords=c("x","y"), crs=st_crs(x)) |>
        filter(st_intersects(geometry, x, sparse=FALSE)[,1L])
    }) |>
    bind_rows() ->
    pts

  if(FALSE){
    ggplot() +
      geom_sf(data=habitat, aes(fill=Habitat)) +
      geom_sf(data=pts, size=0.1) +
      coord_sf(xlim=c(9,9.5), ylim=c(54.8,55), crs="WGS84")
  }


  #' Then do a kernal density estimation:
  coords <- st_coordinates(pts)
  bb <- st_bbox(pts)

  #' The x/y dimensions depend on square vs hexagon:
  if(raster_shape=="squares"){

    ## Convert from km2 to m2 and get square edges:
    dx=dy <- floor(sqrt(as.numeric(set_units(raster_size, "m^2"))))
    bb[c(1,2)] <- floor(bb[c(1,2)]/c(dx,dy))*c(dx,dy)
    bb[c(3,4)] <- ceiling(bb[c(3,4)]/c(dx,dy))*c(dx,dy)
    ns <- c((bb["xmax"]-bb["xmin"])/dx +1,(bb["ymax"]-bb["ymin"])/dy +1)
    cellsize <- c(dx,dy)
    cell_area <- dx*dy
    offset <- c(bb["xmin"]-(dx/2), bb["ymin"]-(dy/2))
    stopifnot(dx==dy)
    is_square <- TRUE

  }else if(raster_shape=="hexagons"){

    # dx <- 1155  # Make dy as close to an integer as possible
    dx <- floor(sqrt(2/3 * 1/sqrt(3) * as.numeric(set_units(raster_size, "m^2"))) * sqrt(3))
    # https://www.gigacalculator.com/calculators/hexagon-calculator.php
    dy <- (3/2)*(dx/sqrt(3))
    bb[c(1,2)] <- floor(bb[c(1,2)]/c(dx,dy))*c(dx,dy)
    bb[c(3,4)] <- ceiling(bb[c(3,4)]/c(dx,dy))*c(dx,dy)
    ns <- round(c((bb["xmax"]-bb["xmin"])/dx +1,(bb["ymax"]-bb["ymin"])/dy +1))
    cellsize <- dx
    cell_area <- 3/2 * sqrt(3) * (cellsize/sqrt(3))^2
    offset <- c(bb["xmin"], bb["ymin"])
    is_square <- FALSE

  }else{
    stop("Unrecognised raster_shape")
  }

  bw <- c(MASS::bandwidth.nrd(coords[,1]), MASS::bandwidth.nrd(coords[,2]))
  #' TODO: the value chosen for h affects the extent of smoothing (currently half the default value)
  #' TODO: for hexagons scale y h so that it is equivalent to x h i.e. accounts for aspect ratio
  dens <- MASS::kde2d(coords[,1], coords[,2], h=bw*h_adj, n=ns, lims=bb[c(1,3,2,4)])
  stopifnot(all(dim(dens$z)==ns))
  # image(dens)

  dens_z <- dens[["z"]]
  if(!is_square){
    ## Average density of even numbered rows with their right hand neighbour (except the last column, which we just leave)
    is_even <- seq(1,nrow(dens_z)) %% 2 == 0L
    for(c in 1:(ncol(dens_z)-1)){
      dens_z[is_even,c] <- (dens_z[is_even,c] + dens_z[is_even,c+1]) / 2
    }
  }

  tibble(geometry = st_make_grid(st_as_sfc(bb), cellsize=cellsize, offset=offset, square=is_square)) |>
    st_as_sf() |>
    mutate(centroid = st_centroid(geometry)) ->
    patches

  tibble(y = dens[["y"]]) |> mutate(row = rep(c("odd","even"), ceiling(length(dens[["y"]])/2))[1:n()]) |>
    expand_grid(x = dens[["x"]]) |>
    mutate(z = dens_z |> as.numeric()) ->
    density

  if(!is_square){
    patches |>
      filter(st_intersects(centroid, st_as_sfc(bb+c(-dy*0.1,-dy*1.1,dx*1.1,dy*0.1)), sparse=FALSE)[,1L]) ->
      patches

    density |>
      mutate(x = case_when(
        row=="odd" ~ x,
        TRUE ~ x + (dx/2)
      )) |>
      identity() ->
      density
  }


  stopifnot(nrow(patches)==nrow(density))
  index <- st_intersects(patches[["geometry"]], density |> st_as_sf(coords=c("x","y"), crs=st_crs(pts))) |> as.numeric()
  stopifnot(length(index)==nrow(patches))
  bind_cols(patches, density[index,]) |>
    filter(st_intersects(geometry, habitat |> pull(geometry) |> st_union(), sparse=FALSE)[,1L]) |>
    mutate(geometry = st_intersection(geometry, habitat |> pull(geometry) |> st_union())) |>
    mutate(area = st_area(geometry)) |>
    identity() ->
    density

  out_density <<- density

  #plot(st_coordinates(density[["centroid"]])[,1], density[["x"]]); abline(0,1)
  #plot(st_coordinates(density[["centroid"]])[,2], density[["y"]]); abline(0,1)

  if(FALSE){
    ggplot(density, aes(fill=z)) + geom_sf()
    #ggplot(density, aes(col=z, fill=z)) + geom_sf()
    ggplot(habitat, aes(fill=Habitat)) + geom_sf()
  }


  # We need to do 2 passes here;
  ## first to find patches of sufficient sizes, then filter eligible cells
  ## second to get final patches

  ## Set an inclusion threshold for the z so that we end up with the same total area of habitat as the raw data
  density |>
    as_tibble() |>
    arrange(desc(z)) |>
    #  mutate(area = as.numeric(area, units="km2"))
    mutate(Delta = abs(cumsum(area)-total_size)) |>
    arrange(Delta) |>
    slice(1L) |>
    pull(z) ->
    target_z

  ## Then filter out based on density, and convert to polygons:
  density |>
    filter(z >= target_z) |>
    pull(geometry) |>
    st_union() |>
    st_cast("POLYGON") |>
    as_tibble() |>
    st_as_sf() |>
    mutate(PatchID = 1:n(), Area = st_area(geometry)) |>
    filter(Area >= min_size) |>
    st_union() ->
    eligible_patches

  ## Now work out which cells are within or bordering these polygons:
  density |>
    filter(st_intersects(geometry, eligible_patches, sparse=FALSE)[,1L]) ->
    eligible_density

  if(FALSE){
    ggplot(eligible_density) + geom_sf()
  }

  ## Set a new inclusion threshold for the z:
  eligible_density |>
    as_tibble() |>
    arrange(desc(z)) |>
    #  mutate(area = as.numeric(area, units="km2"))
    mutate(Delta = abs(cumsum(area)-total_size)) |>
    arrange(Delta) |>
    slice(1L) |>
    pull(z) ->
    target_z

  ## Then filter out based on density, and convert to polygons:
  eligible_density |>
    filter(z >= target_z) |>
    pull(geometry) |>
    st_union() |>
    st_cast("POLYGON") |>
    as_tibble() |>
    st_as_sf() |>
    mutate(PatchID = 1:n(), Area = st_area(geometry)) ->
    patches

  if(FALSE){
    tarea <- patches |> as_tibble() |> summarise(Area=sum(Area)) |> pull(Area)
    units(tarea) <- "km^2"
    tarea
    ggplot() + geom_sf(data=load_map("DK032")) + geom_sf(data=patches, fill="blue")
    ggplot(habitat, aes(fill=Habitat)) + geom_sf() + theme(legend.pos="none")
  }


  ## Then put points back into the included areas and do k-means clustering to break apart larger patches:
  by <- 1/sqrt(as.numeric(set_units(point_density, "1/m^2")))
  expand_grid(x=seq(bb["xmin"], bb["xmax"], by=by), y=seq(bb["ymin"], bb["ymax"], by=by)) |>
    st_as_sf(coords=c("x","y"), crs=st_crs(habitat)) |>
    mutate(PatchID = st_intersects(geometry, patches, sparse=TRUE) |> as.numeric()) |>
    filter(!is.na(PatchID)) ->
    points

  if(FALSE){
    ggplot() + geom_sf(data=load_map("DK032")) + geom_sf(data=points, col="blue", size=0.1)
    summary(points)
  }

  ## Split according to max size
  points |>
    group_split(PatchID) |>
    pblapply(function(x){
      pid <- x[["PatchID"]][1L]
      area <- patches[["Area"]][pid]
      split <- ceiling(area / max_size) |> as.numeric()

      if(split==1L){
        patches |>
          filter(PatchID==pid) |>
          mutate(SubPatch = 1L, MainPatch = PatchID) |>
          select(geometry, PatchID, MainPatch, SubPatch, Area) ->
          rv
      }else{
        clstr <- kmeans(x[["geometry"]] |> st_coordinates(), centers=split,
                        iter.max=50L, algorithm = "Hartigan-Wong")
        clstr[["centers"]] |>
          st_multipoint() |>
          st_voronoi(envelope = patches |> filter(PatchID==pid) |> st_as_sfc()) |>
          st_collection_extract() ->
          voronois

        tibble(geometry=voronois) |>
          st_as_sf(crs=st_crs(x)) |>
          mutate(geometry = st_intersection(geometry, patches |> filter(PatchID==pid))) |>
          mutate(PatchID = pid, MainPatch = pid, SubPatch = 1:n(), Area = st_area(geometry)) ->
          rv
      }
      rv
    }) |>
    bind_rows() |>
    mutate(PatchID = str_c(MainPatch, "_", SubPatch),
           Capacity = (set_units(Area, "km^2")*patch_density) |> as.numeric()) |>
    select(geometry, PatchID, Capacity) ->
    new_patches

  if(FALSE){
    ggplot() + geom_sf(data=load_map("DK032")) + geom_sf(data=new_patches, fill="light blue")

    sum(new_patches[["Capacity"]])
    total_habitat |> set_units("km^2") |> as.numeric()
  }

  new_patches

}
