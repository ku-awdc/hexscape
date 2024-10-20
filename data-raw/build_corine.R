library("hexscape")
library("pbapply")

corine_year <- "2018"
map_year <- "2021"

corine_path <- "~/Documents/Resources/Datasets/Corine/u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"
out_path <- "~/Desktop/corine"
if(!dir.exists(file.path(out_path, corine_year))) dir.create(file.path(out_path, corine_year), recursive=TRUE)

an1 <- c("DK0", "LU0", all_nuts_codes(level=1L, pattern="UK")[["NUTS"]])
pblapply(an1, function(n1){
  path <- read_corine(n1, map_year, corine_path, max_rows=5e5, verbose=0L)
  file.copy(path, file.path(out_path,corine_year), overwrite=TRUE)
  ## So we can test downloads:
  file.remove(path)
})

## Corine info file:
tibble(
  NUTS1 = all_nuts_codes(level=1L)[["NUTS"]],
  CorineYear = corine_year,
  MapYear = map_year
) |>
  mutate(Link = if_else(NUTS1 %in% an1, str_c("https://www.costmodds.org/rsc/hexscape/corine/", corine_year, "/", NUTS1, ".rqs"), "")) |>
  mutate(Comment = if_else(NUTS1 %in% an1, as.character(Sys.Date()), "CORINE data is currently only available for Denmark, Luxembourg and UK. We are working on finding a hosting solution for all CORINE data, but in the meantime see ?save_corine")) ->
  info
qs::qsave(info, file.path(out_path, "info.rqs"), preset="archive")


## TODO: st_buffer and st_simplify on map???

## Note: 5e5 rows is fastest for DK0 (similar to Inf); reduced compresses to same size as minimal
## but is probably easier to filter for specific NUTS2/3/4 areas?!?
map <- load_map("DK0")
corine_raw <- extract_corine(map, corine_path, type="reduced", intersection=TRUE, max_rows=0, verbose=1L)
corine_raw <- extract_corine(map, corine_path, type="minimal", intersection=TRUE, max_rows=5e5, verbose=1L)
corine_raw <- extract_corine(map, corine_path, type="minimal", intersection=TRUE, max_rows=Inf, verbose=1L)
