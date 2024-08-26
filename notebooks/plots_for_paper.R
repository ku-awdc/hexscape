library("hexscape")
library("sf")

hexscape::nuts_codes |> filter(Country=="Denmark")

dk <- load_map(hexscape::nuts_codes |> filter(Country=="Denmark") |> pull(NUTS))
ggplot(dk, aes(fill=NUTS)) + geom_sf()

sj <- load_map("DK032")
ggplot(sj) + geom_sf()

load_corine(c("DK032")) |>
  mutate(Habitat = case_when(
    CLC_Label2=="Forests" ~ "High",
    CLC_Label2=="Scrub and/or herbaceous vegetation associations" ~ "Low",
    .default = "Non"
  )) |>
  group_by(NUTS, Habitat) |>
  summarise(Area=sum(Area), Area_simplified=sum(Area_simplified), geometry=st_union(geometry), .groups="drop") |>
  mutate(Density = case_when(
    Habitat == "High" ~ 0.75,
    Habitat == "Low" ~ 0.25,
    .default = 0.0
  )) ->
  sjc


ggplot() + geom_sf(aes(col=Habitat, fill=Habitat), sjc |> filter(Habitat!="Non") |> mutate(Habitat = factor(Habitat, rev(c("Low","High"))))) + geom_sf(aes(), sj, fill="transparent")



hexscape::nuts_codes |> filter(Country=="France") |> print(n=Inf)

fr <- load_map(c("FRJ","FRK"))
ggplot(fr) + geom_sf()


load_corine(c("FRJ","FRK")) |>
  as_tibble() |>
  group_by(CLC_Label1, CLC_Label2, CLC_Label3) |>
  summarise(Area = sum(Area), AreaS = sum(Area_simplified), .groups="drop") |>
  mutate(AreaP = Area / sum(Area)) |>
  arrange(desc(Area)) ->
  frs

load_corine(c("FRJ","FRK")) |>
  mutate(Habitat = case_when(
    CLC_Label2=="Forests" ~ "High",
    CLC_Label2=="Scrub and/or herbaceous vegetation associations" ~ "Low",
    .default = "Non"
  )) |>
  group_by(NUTS, Habitat) |>
  summarise(Area=sum(Area), Area_simplified=sum(Area_simplified), geometry=st_union(geometry), .groups="drop") |>
  mutate(Density = case_when(
    Habitat == "High" ~ 0.75,
    Habitat == "Low" ~ 0.25,
    .default = 0.0
  )) ->
  frc

frc |>
  as_tibble() |>
  group_by(Habitat) |>
  summarise(Area = sum(Area)) |>
  mutate(AreaP = Area / sum(Area))

ggplot() + geom_sf(aes(col=Habitat, fill=Habitat), frc |> filter(Habitat!="Non") |> mutate(Habitat = factor(Habitat, rev(c("Low","High"))))) + geom_sf(aes(), fr, fill="transparent")

hexscape::nuts_codes |>
  filter(Country=="France", Level==3, str_detect(NUTS, "FRJ") | str_detect(NUTS, "FRK")) |>
  pull(NUTS) |>
  load_map() ->
  frn3
ggplot() + geom_sf(aes(col=Habitat, fill=Habitat), frc |> filter(Habitat!="Non") |> mutate(Habitat = factor(Habitat, rev(c("Low","High"))))) + geom_sf(aes(), frn3, fill="transparent")

patches <- discretise_habitat(frc, max_size=5, min_size=0.5, raster_size=0.05, verbose=3L)
