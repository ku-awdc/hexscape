# Example of using Voronoi tesselation to help randomise farm locations

library("tidyverse")
library("sf")

set.seed(2022-09-22)

## Extract land usage and fake farm data:

# Use hexscape for land usage data:
if(file.exists("hexscape_extract.rda")){
  (load("hexscape_extract.rda"))
}else{
  library("hexscape")
  set_storage_folder("~/Documents/Resources/Datasets/hexscape")
  map_dk <- extract_map("DK")
  extract_corine("DK", verbose=2L) |>
    filter(CLC_LABEL1 == "Agricultural areas") |>
    st_union() ->
    farmland
  save(map_dk, farmland, file="hexscape_extract.rda")
}

# Filter to south Jutland:
map_dk |>
  filter(NUTS_NAME == "Sydjylland") ->
  map_sj
farmland |>
  st_intersection(map_sj) ->
  farmland_sj

# Some fake "true" farm locations for 1000 farms:
tibble(Farm = 1:1000) |>
  mutate(geometry = st_sample(farmland_sj, n())) |>
  st_as_sf(sf_column_name="geometry") ->
  farms

# Map with farmland and farm locations:
ggplot() +
  geom_sf(data=map_sj) +
  geom_sf(data=farmland_sj, fill="dark green", col="dark green") +
  geom_sf(data=farms, col="dark red", size=0.5) +
  theme_void()


## Compute Voronoi tesselation:
voronoi_tesselation <- st_collection_extract(st_voronoi(st_union(farms$geometry)))

# Annoyingly, the ordering of the polygons does not map to the input geometries
# (see https://github.com/r-spatial/sf/issues/1371), so we must do an intersect:
vt_index <- as.numeric(st_intersects(farms$geometry, voronoi_tesselation, sparse=TRUE))
stopifnot(length(vt_index)==nrow(farms))

# Then intersect with farmland and calculate area:
farms |>
  mutate(Voronoi = voronoi_tesselation[vt_index]) |>
  mutate(Voronoi = st_intersection(Voronoi, farmland_sj)) |>
  mutate(Area = st_area(Voronoi)) ->
  farms

# Now each farm is associated with its own Voronoi tesselation polygon
ggplot() +
  geom_sf(data=map_sj) +
  geom_sf(data=farms, aes(geometry=Voronoi), col="dark green", fill="dark green", alpha=0.5) +
  geom_sf(data=farms, col="dark red", size=0.5) +
  theme_void()

# And we have an area of each polygon:
summary(farms)


## So to resample farm locations we can do the following:

# Find the closest N neighbours to each farm:
N <- 5L
expand_grid(Farm = unique(farms$Farm), Neighbour = unique(farms$Farm)) |>
  mutate(Distance = as.numeric(st_distance(farms))) |>
  group_by(Farm) |>
  arrange(Distance) |>
  slice_head(n=N+1) |>  # Note: +1 as we include the original farm as well
  ungroup() |>
  left_join(farms, by=c(Neighbour="Farm")) ->
  resampling

# Then for each farm sample a new location by first choosing which of
# the neighbouring Voronoi polygons to use, and then using st_sample
# within that polygon:
resampling |>
  group_by(Farm) |>
  slice_sample(n=1, weight_by=Area) |>
  ungroup() |>
  mutate(NewLocation = st_sample(Voronoi, rep(1,n()), by_polygon=TRUE, progress=TRUE)) ->
  farms_random
# Note: the method used to do st_sample is quite inefficient: we can
# maybe do better by rasterising, and/or sampling for many areas simultaneously
# and then discarding what we don't use???
# Also, it would probably make sense to ensure that farms are not too close to
# each other, possibly by using st_buffer somehow (not currently sure
# how to do this efficiently!)

# Add distances between original and resampled farms:
farms_random |>
  mutate(Shift = as.numeric(st_distance(geometry, NewLocation, by_element=TRUE))) ->
  farms_random
summary(farms_random)

# Show old and new locations for the farm with the biggest shift:
farm_num <- farms_random |> arrange(desc(Shift)) |> slice(1) |> pull(Farm)
# Or pick a random farm:
farm_num <- sample(1:1000, 1)
ggplot() +
  geom_sf(data=map_sj) +
  geom_sf(data=farms |> filter(Farm==farm_num), mapping=aes(geometry=Voronoi), fill="dark green", alpha=0.25) +
  geom_sf(data=farms |> filter(Farm==farm_num), mapping=aes(geometry=geometry), col="dark green", size=0.5) +
  geom_sf(data=farms_random |> filter(Farm==farm_num), mapping=aes(geometry=Voronoi), fill="dark red", alpha=0.25) +
  geom_sf(data=farms_random |> filter(Farm==farm_num), mapping=aes(geometry=NewLocation), col="dark red", size=0.5) +
  theme_void()

# All randomised farm locations:
ggplot() +
  geom_sf(data=map_sj) +
  geom_sf(data=farms_random, mapping=aes(geometry=NewLocation), col="dark red", size=0.5) +
  theme_void()


