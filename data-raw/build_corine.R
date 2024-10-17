year <- "2018"

corine_path <- "~/Documents/Resources/Datasets/Corine/u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"

map <- load_map("DK0")
corine_raw <- extract_corine(map, corine_path, verbose=1L)

