library("hexscape")
library("sf")
library("ggplot2")

download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_BN_60M_2016_3035.geojson", "paper/temp.json")

download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_2021_3035.geojson", "paper/nuts.json")
download.file("https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson", "paper/lau.json")

## Bigger as it contains multiple formats, and also not the preferred API:
download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-01m.shp.zip", "paper/nuts.zip")

## Note:  LAU had multiple levels up to 2016 (https://ec.europa.eu/eurostat/web/nuts/local-administrative-units)

nuts <- geojsonsf::geojson_sf("paper/nuts.json", input = "EPSG:3085", wkt = st_crs(3085)$wkt)
lau <- geojsonsf::geojson_sf("paper/lau.json", input = "EPSG:3085", wkt = st_crs(3085)$wkt)

ggplot(nuts |> filter(CNTR_CODE=="DK")) + geom_sf()
ggplot(lau |> filter(CNTR_CODE=="DK")) + geom_sf()

ggplot() + geom_sf(data=lau |> filter(CNTR_CODE=="DK")) + geom_sf(data=nuts |> filter(CNTR_CODE=="DK"), col="red", fill="transparent")

st_as_sf(nuts)
st_as_sf(lau)

hexscape::nuts_codes |>
  filter(Country=="Germany", Level>=3) |>
  pull(NUTS) |>
  load_map() |>
  object.size() |>
  {\(x) x/1e6}()
hexscape::nuts_codes |>
  filter(Country=="Germany", Level<3) |>
  pull(NUTS) |>
  load_map() |>
  object.size() |>
  {\(x) x/1e6}()


uk <- bind_rows(
  load_map(hexscape::nuts_codes |> filter(Country=="United Kingdom", Level==1) |> pull(NUTS)) |> as_tibble(),
  load_map(hexscape::nuts_codes |> filter(Country=="Ireland", Level==1) |> pull(NUTS)) |> as_tibble()
)
ggplot(uk, aes(fill=NUTS, geometry=geometry)) + geom_sf() + theme_void() + theme(legend.title = element_blank(), legend.position="bottom")
ggplot(uk, aes(fill=NUTS, geometry=geometry)) + geom_sf(col="transparent") + theme_void() + theme(legend.title = element_blank(), legend.position="right") + labs(caption="© EuroGeographics for the administrative boundaries")
ggsave("paper/figure_1.pdf", width=5, height=5)

## Should be:
map_data <- load_map(c("UK","IE"), nuts_level=1)
autoplot(map_data)


hexscape::nuts_codes |> filter(Country=="Denmark", Level==3)

dk <- load_map(hexscape::nuts_codes |> filter(Country=="Denmark", Level==3) |> pull(NUTS))
ggplot(dk, aes(fill=NUTS)) + geom_sf(col="transparent") + geom_sf(col="transparent") + theme_void() + theme(legend.title = element_blank(), legend.position="bottom") + labs(caption="© EuroGeographics for the administrative boundaries")
ggsave("paper/figure_2.pdf", width=5, height=5)


## Should be:
map_data <- load_map("DK", nuts_level=3)
autoplot(map_data, legend="bottom")

## TODO:  non-simplified next to simplified, also use cols
load_corine(c("DK014")) |>
  ggplot(aes(fill=CLC_Label3)) +
  geom_sf() +
  theme_void() + theme(legend.title = element_blank(), legend.position="none") + labs(caption="Copernicus Land Monitoring Service products and services\nwere produced with funding by the European Union") +
  scale_fill_manual()

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


load_corine(c("LU")) |>
  mutate(Habitat = case_when(
    CLC_Label3=="Coniferous forest"~"Low",
    CLC_Label2=="Forests" ~ "High",
    CLC_Label2=="Scrub and/or herbaceous vegetation associations" ~ "MediumS",
    CLC_Label1=="Agricultural areas" ~ "MediumA",
    .default = "Non"
  )) |>
  group_by(NUTS, Habitat) |>
  summarise(Area=sum(Area), Area_simplified=sum(Area_simplified), geometry=st_union(geometry), .groups="drop") ->
  luc
ggplot(luc) + geom_sf(aes(fill=Habitat))


lu <- load_map(hexscape::nuts_codes |> filter(Country=="Luxembourg", Level==3) |> pull(NUTS))
ggplot(lu, aes(fill=NUTS)) + geom_sf(col="transparent") + geom_sf(col="transparent") + theme_void() + theme(legend.title = element_blank(), legend.position="bottom") + labs(caption="© EuroGeographics for the administrative boundaries")



hexscape::nuts_codes |> filter(Country=="France") |> print(n=Inf)

mi <- load_map(hexscape::nuts_codes |> filter(str_detect(NUTS, "FRJ") | str_detect(NUTS, "FRK"), Level==3) |> pull(NUTS))
ggplot(mi, aes(fill=NUTS)) + geom_sf()

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
