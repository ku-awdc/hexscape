
library("hexscape")
library("sf")
library("units")
library("pbapply")


aggregate_corine <- function(corine, map, min_size, max_size, points, bw){

}

#' TODO:  look at ?st_interpolate_aw to simplify first stage (Corine -> Hexagons) - density version

#' Take corine data and simplify to non-habitable / low-habitable / high-habitable:
load_corine("DK032") |>
  mutate(Habitat = case_when(
    CLC_Label2=="Forests" ~ "High",
    CLC_Label2=="Scrub and/or herbaceous vegetation associations" ~ "Low",
    TRUE ~ "Non"
  )) |>
  group_by(NUTS, Habitat) |>
  summarise(Area=sum(Area), Area_simplified=sum(Area_simplified), geometry=st_union(geometry), .groups="drop") ->
  habitat

ggplot(habitat, aes(fill=Habitat)) + geom_sf()
ggplot(habitat, aes(fill=Habitat)) + geom_sf() + coord_sf(xlim=c(9,9.5), ylim=c(54.8,55), crs="WGS84")


#' Then add artificial points representing habitat suitability (4x the density for high vs low):
bb <- st_bbox(habitat)
bind_rows(
  expand_grid(x=seq(bb["xmin"], bb["xmax"], by=100), y=seq(bb["ymin"], bb["ymax"], by=100)) |>
    st_as_sf(coords=c("x","y"), crs=st_crs(habitat)) |>
    filter(st_intersects(geometry, habitat |> filter(Habitat=="High"), sparse=FALSE)[,1L]),
  expand_grid(x=seq(bb["xmin"], bb["xmax"], by=200), y=seq(bb["ymin"], bb["ymax"], by=200)) |>
    st_as_sf(coords=c("x","y"), crs=st_crs(habitat)) |>
    filter(st_intersects(geometry, habitat |> filter(Habitat=="Low"), sparse=FALSE)[,1L])
) ->
  pts
#' TODO:  are these densities sensible??  Add argument.

ggplot() +
  geom_sf(data=habitat, aes(fill=Habitat)) +
  geom_sf(data=pts, size=0.1) +
  coord_sf(xlim=c(9,9.5), ylim=c(54.8,55), crs="WGS84")


#' Then do a kernal density estimation:
type <- "hexagons"
coords <- st_coordinates(pts)
bb <- st_bbox(pts)

#' The x/y dimensions depend on square vs hexagon:
if(type=="squares"){

  dx <- 1000
  dy <- 1000
  bb[c(1,2)] <- floor(bb[c(1,2)]/c(dx,dy))*c(dx,dy)
  bb[c(3,4)] <- ceiling(bb[c(3,4)]/c(dx,dy))*c(dx,dy)
  ns <- c((bb["xmax"]-bb["xmin"])/dx +1,(bb["ymax"]-bb["ymin"])/dy +1)
  cellsize <- c(dx,dy)
  cell_area <- dx*dy
  offset <- c(bb["xmin"]-(dx/2), bb["ymin"]-(dy/2))
  stopifnot(dx==dy)
  is_square <- TRUE

}else if(type=="hexagons"){

  dx <- 1155  # Make dy as close to an integer as possible
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
  stop("Unrecognised type")
}

bw <- c(MASS::bandwidth.nrd(coords[,1]), MASS::bandwidth.nrd(coords[,2]))
#' TODO: the value chosen for h affects the extent of smoothing (currently half the default value)
#' TODO: for hexagons scale y h so that it is equivalent to x h i.e. accounts for aspect ratio
dens <- MASS::kde2d(coords[,1], coords[,2], h=min(bw)*0.5, n=ns, lims=bb[c(1,3,2,4)])
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
# ggplot(patches) + geom_sf()


tibble(y = dens[["y"]]) |> mutate(row = rep(c("odd","even"), ceiling(length(dens$y)/2))[1:n()]) |>
  expand_grid(x = dens[["x"]]) |>
  mutate(z = dens_z |> as.numeric()) ->
  density


## Alternative (doesn't work):
# int <- st_interpolate_aw(density |> st_as_sf(coords=c("x","y"), crs=st_crs(patches)) |> select(z), patches, extensive=FALSE)



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
  filter(st_intersects(geometry, habitat |> pull(geometry) |> st_union(), sparse=FALSE)) |>
  mutate(geometry = st_intersection(geometry, habitat |> pull(geometry) |> st_union())) |>
  mutate(area = st_area(geometry)) |>
  identity() ->
  density


#plot(st_coordinates(density[["centroid"]])[,1], density[["x"]]); abline(0,1)
#plot(st_coordinates(density[["centroid"]])[,2], density[["y"]]); abline(0,1)

ggplot(density, aes(fill=z)) + geom_sf()
#ggplot(density, aes(col=z, fill=z)) + geom_sf()
ggplot(habitat, aes(fill=Habitat)) + geom_sf()


## Then set an inclusion threshold for the z so that we end up with the same total area of habitat as the raw data
habitat |>
  as_tibble() |>
  filter(Habitat %in% c("Low","High")) |>
  summarise(Area = sum(Area)) |>
  pull(Area) ->
  target_area
units(target_area) <- "km^2"
#' TODO: distinguish between high and low density habitat

density |>
  as_tibble() |>
  arrange(desc(z)) |>
#  mutate(area = as.numeric(area, units="km2"))
  mutate(Delta = abs(cumsum(area)-target_area)) |>
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
  mutate(PatchID = 1:n(), Area = st_area(geometry)) ->
  patches

patches |> as_tibble() |> summarise(Area=sum(Area)/1e6)
habitat |> as_tibble() |> summarise(Area=sum(Area * if_else(Habitat=="Non", 0, 1)))

ggplot() + geom_sf(data=load_map("DK032")) + geom_sf(data=patches, fill="blue")
ggplot(habitat, aes(fill=Habitat)) + geom_sf() + theme(legend.pos="none")

#' TODO: Detect blocks less than a specified size and remove / replace with other squares/hexagons that are neighbours of existing areas?
# patches |>
#   mutate(Area = st_area(geometry)) |>
#   mutate(Included = Area >= 5*cell_area)
#
# st_multipolygon(patches$geometry) |>
#   lapply(function(x) x) |>
#   st_sfc(crs=st_crs(patches)) |>
#   as_tibble() |>
#   mutate(ClusterID = 1:n(), Size = st_area(geometry))


## Then put points back into the included areas and do k-means clustering to break apart larger patches:
expand_grid(x=seq(bb["xmin"], bb["xmax"], by=1000), y=seq(bb["ymin"], bb["ymax"], by=1000)) |>
  st_as_sf(coords=c("x","y"), crs=st_crs(habitat)) |>
  mutate(PatchID = st_intersects(geometry, patches, sparse=TRUE) |> as.numeric()) |>
  filter(!is.na(PatchID)) ->
  points

ggplot() + geom_sf(data=load_map("DK032")) + geom_sf(data=points, col="blue", size=0.1)
summary(points)

points |>
  group_split(PatchID) |>
  pblapply(function(x){
    pid <- x[["PatchID"]][1L]
    area <- patches[["Area"]][pid]
    split <- ceiling(area / 5000^2) |> as.numeric()

    if(split==1L){
      patches |>
        filter(PatchID==pid) |>
        mutate(SubPatch = 1L, MainPatch = PatchID) |>
        select(geometry, PatchID, MainPatch, SubPatch, Area) ->
        rv
    }else{
      clstr <- kmeans(x[["geometry"]] |> st_coordinates(), centers=split, algorithm = "Hartigan-Wong")
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
  mutate(PatchID = str_c(MainPatch, "_", SubPatch)) ->
  new_patches

ggplot() + geom_sf(data=load_map("DK032")) + geom_sf(data=new_patches, fill="light blue")

new_patches

# saveRDS(new_patches, "patches.rds")
