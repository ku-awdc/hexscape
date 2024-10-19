library("hexscape")

year <- "2018"

corine_path <- "~/Documents/Resources/Datasets/Corine/u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"
out_path <- "~/Desktop/corine"

an3 <- c("DK0", "LU0", all_nuts_codes(level=1L, pattern="UK")[["NUTS"]])
for(n3 in an3){
  map <- load_map(n3)
  corine_raw <- extract_corine(map, corine_path, type="minimal", intersection=TRUE, max_rows=5e5, verbose=1L)
  attr(corine_raw, "map_year") <- year
  attr(corine_raw, "hexscape_version") <- packageVersion("hexscape")

  ## Save:
  qs::qsave(corine_raw, file.path(out_path, str_c(n3, ".rqs")), preset="archive")
}

## TODO: st_buffer and st_simplify on map???

## Note: 5e5 rows is fastest for DK0 (similar to Inf); reduced compresses to same size as minimal
## but is probably easier to filter for specific NUTS2/3/4 areas?!?
corine_raw <- extract_corine(map, corine_path, type="reduced", intersection=TRUE, max_rows=0, verbose=1L)
corine_raw <- extract_corine(map, corine_path, type="minimal", intersection=TRUE, max_rows=5e5, verbose=1L)
corine_raw <- extract_corine(map, corine_path, type="minimal", intersection=TRUE, max_rows=Inf, verbose=1L)

attr(corine_raw, "map_year") <- year
attr(corine_raw, "hexscape_version") <- packageVersion("hexscape")

## Save:
qs::qsave(corine_raw, str_c(n3, ".rqs"), preset="archive")
