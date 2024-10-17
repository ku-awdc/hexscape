year <- "2018"

corine_path <- "~/Documents/Resources/Datasets/Corine/u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"

n3 <- "DK0"
map <- load_map(n3)

## TODO: st_buffer and st_simplify on map???
corine_raw <- extract_corine(map, corine_path, verbose=1L)

## Reduce file size by removing unnecessary things:
corine_raw <- corine_raw |> select(CODE_18, Shape)

## Save:
qs::qsave(corine_raw, str_c(n3, ".rqs"), preset="archive")
