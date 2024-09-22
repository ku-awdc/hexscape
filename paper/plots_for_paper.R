library("hexscape")
library("sf")
library("ggplot2")

### Downloading data

download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_BN_60M_2016_3035.geojson", "paper/temp.json")

download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_2021_3035.geojson", "paper/nuts.json")
download.file("https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_3035.geojson", "paper/lau.json")

## Bigger as it contains multiple formats, and also not the preferred API:
download.file("https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-01m.shp.zip", "paper/nuts.zip")

## Note:  LAU had multiple levels up to 2016 (https://ec.europa.eu/eurostat/web/nuts/local-administrative-units)

nuts <- geojsonsf::geojson_sf("~/Desktop/GSICO/nuts.json", input = "EPSG:3035", wkt = st_crs(3035)$wkt)
lau <- geojsonsf::geojson_sf("~/Desktop/GSICO/lau.json", input = "EPSG:3035", wkt = st_crs(3035)$wkt)


## FIGURE 1

fig1dat <- nuts |> filter(CNTR_CODE%in%c("UK","IE"), LEVL_CODE==1) |> mutate(Label = str_c(NUTS_ID, ": ", str_to_title(NUTS_NAME)))
ggplot(fig1dat, aes(fill=Label, geometry=geometry)) +
  geom_sf(col="transparent") + theme_void() +
  theme(legend.title = element_blank(), legend.position="left", legend.text = element_text(size = 6), legend.key.size = unit(0.35,"cm")) +
  #guides(fill = guide_legend(override.aes = list(size = 0.25))) +
  labs(caption="© EuroGeographics for the administrative boundaries")
ggsave("paper/figure_1.pdf", width=5, height=3)


## FIGURE 2

fig2dat <- lau |> filter(CNTR_CODE%in%c("DK")) |> mutate(Label = str_to_title(LAU_NAME))
ggplot(fig2dat, aes(fill=Label, geometry=geometry)) + geom_sf(col="transparent") + theme_void() + theme(legend.title = element_blank(), legend.position="none") + labs(caption="© EuroGeographics for the administrative boundaries")
ggsave("paper/figure_2.pdf", width=5, height=3)


## FIGURE 3
corine_dk <- qs::qread("~/Desktop/GSICO/DK0N16.rqs")
nuts_bh <- nuts |> filter(NUTS_NAME=="Bornholm")
corine_dk |>
  filter(st_intersects(geometry, nuts_bh$geometry, sparse=FALSE)[,1]) |>
  mutate(geometry = st_intersection(geometry, nuts_bh$geometry)) |>
  left_join(
    clc_codes,
    by="CLC"
  ) ->
  bornholm

readLines("~/Desktop/corine_codes_html.txt") |>
  str_c(collapse="\n") |>
  rvest::minimal_html() |>
  rvest::html_element("table") |>
  rvest::html_table() |>
  rename_with(\(x) str_replace(x," ", "")) |>
  filter(RGB!="") |>
  separate(RGB, c("R","G","B"), "-") |>
  mutate(across(c(R,G,B), as.numeric)) |>
  group_by(Level1) |>
  mutate(RGB1 = rgb(mean(R), mean(G), mean(B), maxColorValue = 255)) |>
  ungroup() |>
  mutate(RGB3 = rgb(R,G,B, maxColorValue = 255)) |>
  mutate(CLC = str_sub(Level3, 1L, 5L) |> str_replace_all(fixed("."), "")) ->
  clc_codes

clc_cols <- clc_codes$RGB3
names(clc_cols) <- clc_codes$CLC

p1 <- ggplot(bornholm, aes(fill=CLC)) +
  geom_sf() +
  scale_fill_manual(values=clc_cols) +
  theme_void() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(a)")
p1

clc_cols <- clc_codes$RGB1
names(clc_cols) <- clc_codes$CLC

plot(clc_codes$CLC, col=clc_codes$RGB3)

bornholm |>
  group_by(str_sub(CLC, 1L, 1L)) |>
  summarise(CLC = CLC[1]) ->
  bha

p2 <- ggplot(bha, aes(fill=CLC)) +
  geom_sf() +
  scale_fill_manual(values=clc_cols) +
  theme_void() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(b)")
p2

bha |>
  rmapshaper::ms_simplify(keep=0.25, keep_shapes=TRUE, explode=TRUE) |>
  group_by(CLC) |>
  summarise() ->
  bhs

p3 <- ggplot(bhs, aes(fill=CLC)) +
  geom_sf() +
  scale_fill_manual(values=clc_cols) +
  theme_void() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(c)")
p3

ggplot(bhs, aes(fill=CLC)) +
  geom_sf() +
  scale_fill_manual(values=clc_cols)
clc_codes |> count(Level1)

ggpubr::ggarrange(p1,p2,p3, nrow=1)
ggsave("paper/figure_3.pdf", width=5, height=3)

object.size(bornholm$geometry) / 1024
object.size(bha$geometry) / 1024
object.size(bhs$geometry) / 1024

full_join(
  bha |> mutate(AreaF = st_area(geometry)) |> as_tibble() |> select(CLC, AreaF),
  bhs |> mutate(AreaS = st_area(geometry)) |> as_tibble() |> select(CLC, AreaS),
  by="CLC"
) |>
  mutate(across(starts_with("Area"), \(x) set_units(x, "km^2"))) |>
  left_join(
    clc_codes |> select(Level1, CLC),
    by="CLC"
  ) |>
  mutate(Level1 = str_sub(Level1, 4L, -1L) |> str_to_sentence()) |>
  mutate(across(starts_with("Area"), \(x) as.numeric(x) |> round(2))) |>
  select(Label=Level1, Raw=AreaF, Simplified=AreaS) |>
  mutate(Change = ((Simplified/Raw * 100)-100) |> round(1)) |>
  knitr::kable(format="latex")

# Comparing to raster:
if(FALSE){
  library("stars")
  raster_tiff <- read_stars("~/Documents/Resources/Datasets/Corine/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
  bh_bbox <- st_bbox(st_transform(nuts_bh, st_crs(raster_tiff)))
  bh_tiff <- raster_tiff[bh_bbox]
  st_as_sf(bh_tiff, as_points = FALSE, merge = TRUE) |>
    mutate(Colour = as.character(`U2018_CLC2018_V2020_20u1.tif`)) ->
    raster_polygon
  ggplot(raster_polygon, aes(fill=Colour)) + geom_sf()

  raster_polygon |>
    group_by(Colour) |>
    summarise() |>
    mutate(AreaR = st_area(geometry)) ->
    bhr

  expand_grid(
    bhr |> as_tibble() |> select(Colour, AreaR) |> filter(Colour!="44"),
    bornholm |> mutate(AreaF = st_area(geometry)) |> as_tibble() |> select(CLC, AreaF)
  ) |>
    group_by(Colour) |>
    arrange(abs(AreaR-AreaF)) |>
    slice(1L) |>
    ungroup() |>
    arrange(CLC)

  readxl::read_excel("~/Documents/Resources/Datasets/Corine/clc2000legend.xls") |>
    select(Colour=GRID_CODE, CLC=CLC_CODE) ->
    clc_lookup

  bhr |>
    left_join(
      clc_lookup |> mutate(across(everything(), as.character)),
      by="Colour"
    ) ->
    bhr

  bhr |>
    as_tibble() |>
    select(Colour, AreaR, CLC) |>
    full_join(
      bornholm |> mutate(AreaF = st_area(geometry)) |> as_tibble() |> select(CLC, AreaF),
      by="CLC"
    ) |>
    filter(Colour!="44") |>
    group_by(str_sub(CLC, 1L, 1L)) |>
    summarise(CLC = CLC[1], AreaR=sum(AreaR) |> as.numeric(), AreaF=sum(AreaF) |> as.numeric()) |>
    mutate(Change = ((AreaR/AreaF * 100)-100) |> round(1), AreaR = AreaR/1e6)

  bhr |>
    group_by(str_sub(CLC, 1L, 1L)) |>
    summarise() |>
    pull(geometry) |>
    object.size() |>
    {\(x) x / 1e3}()
}

## FIGURE 4
map_lu <- load_map("LU0")
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

ls <- st_set_crs(map_lu$geometry*rot(0.785398), st_crs(map_lu)) |> st_buffer(as_units(5,"km")) |> st_set_crs(st_crs(map_lu))
sqrs <- st_make_grid(ls, cellsize=as_units(5,"km^2"))*rot(-0.785398)

centre <- st_point(c(6.13,49.61)) |> st_sfc() |> st_set_crs("WGS84") |> st_transform(st_crs(map_lu))

sqrs |>
  st_as_sf() |>
  st_set_crs(st_crs(map_lu)) |>
  filter(st_intersects(x, centre, sparse=FALSE)[,1L]) |>
  mutate(centroid = st_centroid(x)) |>
  mutate(diff = centroid - centre) ->
  sqr

ggplot() +
  geom_sf(data=sqr |> select(-diff)) +
  geom_sf(data=(centre+sqr$diff) |> st_set_crs(st_crs(map_lu)))

ggplot() +
  geom_sf(data=((sqr |> pull(x))-sqr$diff) |> st_set_crs(st_crs(map_lu)), aes(geometry=geometry)) +
  geom_sf(data=(centre) |> st_set_crs(st_crs(map_lu)))

ggplot() +
  geom_sf(data=(sqrs-sqr$diff) |> st_set_crs(st_crs(map_lu)), aes(geometry=geometry)) +
  geom_sf(data=(centre) |> st_set_crs(st_crs(map_lu)))


p1 <- (sqrs-sqr$diff) |>
  st_as_sf() |>
  st_set_crs(st_crs(map_lu)) |>
  filter(st_intersects(x, map_lu, sparse=FALSE)[,1L]) |>
  mutate(x = st_intersection(x, map_lu)) |>
  ggplot() +
  geom_sf() +
  geom_sf(data=centre, size=1) +
  theme_void() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(a)")
p1

ls <- st_set_crs(map_lu$geometry, st_crs(map_lu)) |> st_buffer(as_units(5,"km")) |> st_set_crs(st_crs(map_lu))
hexs <- st_make_grid(ls, cellsize=as_units(5,"km^2"), square=FALSE)

hexs |>
  st_as_sf() |>
  st_set_crs(st_crs(map_lu)) |>
  filter(st_intersects(x, centre, sparse=FALSE)[,1L]) |>
  mutate(centroid = st_centroid(x)) |>
  mutate(diff = centroid - centre) ->
  hex

p2 <- (hexs-hex$diff) |>
  st_as_sf() |>
  st_set_crs(st_crs(map_lu)) |>
  filter(st_intersects(x, map_lu, sparse=FALSE)[,1L]) |>
  mutate(x = st_intersection(x, map_lu)) |>
  ggplot() +
  geom_sf() +
  geom_sf(data=centre, size=1) +
  theme_void() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(b)")
p2

ggpubr::ggarrange(p1,p2,nrow=1)
ggsave("paper/figure_4.pdf", width=5, height=3)


## FIGURE 5
corine_lu <- qs::qread("~/Desktop/GSICO/LU0N16.rqs")
corine_lu |>
  left_join(
    clc_codes,
    by="CLC"
  ) |>
  group_by(str_sub(CLC, 1L, 1L)) |>
  summarise(CLC = CLC[1]) ->
  corine_lu

hexes <- (hexs-hex$diff) |>
  st_as_sf() |>
  st_set_crs(st_crs(map_lu)) |>
  filter(st_intersects(x, map_lu, sparse=FALSE)[,1L]) |>
  mutate(x = st_intersection(x, map_lu))

clc_cols <- clc_codes$RGB1
names(clc_cols) <- clc_codes$CLC

corine_lu |>
  rmapshaper::ms_simplify(keep=0.25, keep_shapes=TRUE, explode=TRUE) |>
  group_by(CLC) |>
  summarise() ->
  corine_lus

ggplot(corine_lus, aes(fill=CLC)) +
  geom_sf() +
  scale_fill_manual(values=clc_cols) +
  theme_void() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(a)")


hexes |>
  mutate(ID = row_number()) |>
  rowwise() |>
  group_split() |>
  pblapply(\(x){
    corine_lus |>
      filter(st_intersects(geometry,x,sparse=FALSE)[,1L]) |>
      mutate(geometry = st_intersection(geometry, x$x), ID = x$ID)
  }) |>
  bind_rows() ->
  ii

p1 <- ggplot(ii, aes(fill=CLC)) +
  geom_sf() +
  scale_fill_manual(values=clc_cols) +
  theme_void() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(a)")
p1

ii |>
  mutate(Area = st_area(geometry)) |>
  mutate(Density = case_when(
    CLC=="311" ~ 1,
    CLC=="211" ~ 0.25,
    .default=0
  )) |>
  group_by(ID) |>
  summarise(geometry = st_union(geometry), TotalArea = sum(Area), Total = sum(Area*Density)) |>
  mutate(Density = as.numeric(Total/TotalArea)) ->
  pp

p2 <- ggplot(pp, aes(geometry=geometry,fill=Density)) +
  geom_sf() +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(b)") +
  scale_fill_gradient(low="white", high="#8ED55D")
p2

ggpubr::ggarrange(p1,p2)
ggsave("paper/figure_5.pdf", width=5, height=3)


## FIGURE 6
# a: land usage again, but directly showing density
# b: tiles with KDE
# c: output polygons

p1 <- corine_lus |>
  mutate(Density = case_when(
    CLC=="311" ~ 1,
    CLC=="211" ~ 0.25,
    .default=0
  )) |>
  ggplot(aes(geometry=geometry, fill=Density)) +
  geom_sf() +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(a)") +
  scale_fill_gradient(low="white", high="#8ED55D")
p1

## Currently out_density is a side effect...
corine_lus |>
  mutate(Density = case_when(
    CLC=="311" ~ 1,
    CLC=="211" ~ 0.25,
    .default=0
  )) |>
  discretise_habitat() ->
  lucl

p2 <- ggplot() +
  geom_sf(data=map_lu, fill="transparent") +
  geom_sf(aes(geometry=geometry, fill=z), data=out_density, col="transparent") +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(b)") +
  scale_fill_gradient(low="white", high="#8ED55D")
p2

p3 <- ggplot() +
  geom_sf(data=map_lu, fill="white") +
  geom_sf(aes(geometry=geometry), data=lucl, fill="#8ED55D") +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(c)")
p3

ggpubr::ggarrange(p1,p2,p3,nrow=1)
ggsave("paper/figure_6.pdf", width=5, height=3)


## FIGURE 7
# a: farm locations with land usage
# b: Voronoi tesselation and new random points
# c: chosen locations (with Voronoi)

set.seed(2024-09-20)
farms <- sample_points(corine_lu |> filter(CLC=="211") |> mutate(Index=1), 50L)
rands <- randomise_voronoi(farms, corine_lu |> filter(CLC=="211"), additional_info = TRUE)

ss <- 0.75

p1 <- ggplot() +
  geom_sf(data=map_lu, fill="grey90") +
  geom_sf(data=corine_lu |> filter(CLC=="211"), fill="#F1D049", col="transparent") +
  geom_sf(data=farms, col="red", size=ss) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(a)")

p2 <- ggplot() +
  geom_sf(data=map_lu, fill="grey90") +
  #geom_sf(aes(geometry=VoronoiUnmasked), data=rands |> mutate(VoronoiUnmasked = st_intersection(VoronoiUnmasked, map_lu)), fill="grey90", col="transparent") +
  geom_sf(aes(geometry=VoronoiUnmasked), data=rands |> mutate(VoronoiUnmasked = st_intersection(VoronoiUnmasked, corine_lu |> filter(CLC=="211"))), fill="#F1D049", col="transparent") +
  geom_sf(aes(geometry=VoronoiUnmasked), data=rands |> mutate(VoronoiUnmasked = st_intersection(VoronoiUnmasked, map_lu)), fill="transparent", col="black") +
#  geom_sf(aes(geometry=Centroid), data=rands |> mutate(Centroid = st_centroid(VoronoiMasked, map_lu)), col="dark blue") +
  geom_sf(data=farms, col="red", size=ss) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(a)")
p2

p3 <- ggplot() +
  geom_sf(data=map_lu, fill="grey90") +
  geom_sf(data=corine_lu |> filter(CLC=="211"), fill="#F1D049", col="transparent") +
  geom_sf(aes(geometry=RandomShift), data=rands) +
  geom_sf(aes(geometry=VoronoiUnmasked), data=rands |> mutate(VoronoiUnmasked = st_intersection(VoronoiUnmasked, map_lu)), fill="transparent", col="black") +
  geom_sf(data=farms, col="red", size=ss) +
  geom_sf(data=rands, col="blue", size=ss) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(b)")

p4 <- ggplot() +
  geom_sf(data=map_lu, fill="grey90") +
  geom_sf(data=corine_lu |> filter(CLC=="211"), fill="#F1D049", col="transparent") +
#  geom_sf(aes(geometry=RandomShift), data=rands) +
#  geom_sf(aes(geometry=VoronoiUnmasked), data=rands |> mutate(VoronoiUnmasked = st_intersection(VoronoiUnmasked, map_lu)), fill="transparent", col="black") +
#  geom_sf(data=farms, col="red", size=ss) +
  geom_sf(data=rands, col="blue", size=ss) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0.5, size=12)) +
  labs(caption="(c)")

ggpubr::ggarrange(p2,p3,p4,nrow=1)
ggsave("paper/figure_7.pdf", width=5, height=3)

save.image("paper/figues_data.rda")



### OLDER STUFF

corine_lus |>
  mutate(Density = case_when(
    CLC=="311" ~ 1,
    CLC=="211" ~ 0.25,
    .default=0
  )) |>
  discretise_habitat() ->
  lucl

pdf("comp.pdf", width=4, height=6)
ggplot() +
  geom_sf(data=corine_lu, aes(fill=CLC))
ggplot() +
  geom_sf(data=corine_lu, aes(fill=CLC)) +
  geom_sf(data=lucl)
dev.off()






bhs |> mutate(Area = st_area(geometry))

readxl::read_excel("~/Desktop/CLC_codes.xlsx") |>
  mutate(Colour = `Color Code`) |>
  separate(Label, c("Code","Label"), sep=" - ") |>
  select(CLC=Code, Label, Colour) ->
  clc_codes


clc_cols <- clc_codes$Colour
names(clc_cols) <- clc_codes$CLC

ggplot(bornholm, aes(fill=CLC)) +
  geom_sf() +
  #theme(legend.position = "none") +
  scale_fill_manual(values=clc_cols)


bh2 <- load_corine("DK014")


pdf("test2.pdf", width=10, height=10)
ggplot(bh2, aes(fill=CLC_Label3)) +
  geom_sf() + theme(legend.position = "none")

ggplot(bornholm, aes(fill=CLC)) +
  geom_sf() + theme(legend.position = "none")
dev.off()


ggplot(bh2, aes(fill=CLC_Label3)) +
  geom_sf() + theme(legend.position = "none")

ggplot(bornholm, aes(fill=CODE_18)) +
  geom_sf() + theme(legend.position = "none")
+
  theme_void() + theme(legend.title = element_blank(), legend.position="none") + labs(caption="Copernicus Land Monitoring Service products and services\nwere produced with funding by the European Union") +
  scale_fill_manual()



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
