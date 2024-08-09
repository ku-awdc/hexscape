
library("hexscape")
library("sf")
library("units")
library("pbapply")
library("checkmate")



#' Take corine data and simplify to non-habitable / low-habitable / high-habitable:
load_corine("DK032") |>
  mutate(Habitat = case_when(
    CLC_Label2=="Forests" ~ "High",
    CLC_Label2=="Scrub and/or herbaceous vegetation associations" ~ "Low",
    TRUE ~ "Non"
  )) |>
  group_by(NUTS, Habitat) |>
  summarise(Area=sum(Area), Area_simplified=sum(Area_simplified), geometry=st_union(geometry), .groups="drop") |>
  mutate(Density = case_when(
    Habitat == "High" ~ 0.75,
    Habitat == "Low" ~ 0.25,
    .default = 0.0
  )) ->
  habitat

ggplot(habitat, aes(fill=Habitat)) + geom_sf()
ggplot(habitat, aes(fill=Habitat)) + geom_sf() + coord_sf(xlim=c(9,9.5), ylim=c(54.8,55), crs="WGS84")

c(1, 2, 5, 10, 25) |>
  set_names(function(x) str_c("Size_", x, "km2")) |>
  pblapply(function(x){
    discretise_habitat(habitat, max_size=x, min_size=0.5, patch_density=0.75, raster_size=0.05, verbose=0L)
  }, cl=6L) ->
  all_patches

all_patches |>
  lapply(function(x) x |> as_tibble() |> summarise(Capacity = sum(Capacity)))

saveRDS(all_patches, file="aggregated_patches.rds")

dk032 <- load_map("DK032")
pdf("aggregated_patches.pdf")
seq_along(all_patches) |>
  lapply(function(x){
    print({
      ggplot() + geom_sf(data=dk032) + geom_sf(data=all_patches[[x]], fill="forestgreen", col="white") + theme_void() + labs(subtitle=names(all_patches)[x])
    })
  }) ->
  tt
dev.off()


