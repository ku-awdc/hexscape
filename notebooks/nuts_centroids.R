## Calculate centroids for all NUTS codes
library("hexscape")
library("pbapply")
library("sf")

nuts_codes |>
  group_split(NUTS) |>
  pblapply(function(x){
    load_map(x$NUTS) |>
      mutate(centroid = st_transform(centroid, crs=st_crs("WGS84"))) |>
      mutate(Lat=st_coordinates(centroid)[,2], Long=st_coordinates(centroid)[,1]) |>
      as_tibble() |>
      select(NUTS, Level, Country, Code, Label, Lat, Long)
  }) |>
  bind_rows() ->
  nuts_centroids

library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot() + geom_sf(data=world) + geom_sf(data=nuts_centroids |> st_as_sf(coords=c("Long","Lat"), crs="WGS84"), col="red")

write_csv(nuts_centroids, "nuts_centroids.csv")
