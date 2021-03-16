### Code to play around with corine data

# https://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2/corine-land-cover-classes-and/clc_legend.csv
# https://land.copernicus.eu/user-corner/technical-library/corine-land-cover-nomenclature-guidelines/docs/pdf/CLC2018_Nomenclature_illustrated_guide_20190510.pdf

library('tidyverse')
library("sf")
library("eurostat")
library("pbapply")

## We are only interested in Estonia, Denmark and Sweden:

map <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = 'all', year = 2016) %>%
  st_transform(crs= 25832) %>%
  filter(CNTR_CODE %in% c('DK','EE','SE'), LEVL_CODE == 3)

ggplot(map) + geom_sf()

mapsf <- st_union(map$geometry)

if(Sys.info()["user"] == "matthewdenwood")
  corine_path <- "/Users/matthewdenwood/Documents/Resources/Datasets/Corine/u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg"

stopifnot(file.exists(corine_path))
layers <- st_layers(corine_path)
layers

## TODO: make sure there aren't any non-recognised codes in any layer, then use the CSV file
codes <- st_read(corine_path, query = str_c("SELECT DISTINCT Code_18 FROM ", layers$name[1]), layer=layers$name[1])
codes

get_corine_sf <- function(code){
  
  ## First extract the code and filter to only those that intersect anything we are interested in:
  obj <- layers$name %>%
    `names<-`(.,.) %>%
    as.list() %>%
    lapply(function(l) st_read(corine_path, query = str_c("SELECT * FROM ", l, " WHERE Code_18 = ", code), layer=l, quiet=TRUE) %>% mutate(Layer = l)) %>%
    map_df( ~ .x %>% `colnames<-`(., case_when(colnames(.) %in% c("Shape", "Layer") ~ colnames(.), TRUE ~ toupper(colnames(.))))) %>%
    st_transform(st_crs(map)) %>%
    mutate(use = st_intersects(Shape, mapsf, sparse=FALSE)[,1]) %>%
    filter(use) %>%
    select(-use)
  
  ## Then make intersections with each of the NUTS3 areas we have:
  o2 <- map %>%
    split(.$NUTS_ID) %>%
    map( 
      ~ obj %>%
      mutate(use = st_intersects(Shape, .x$geometry, sparse=FALSE)[,1]) %>%
      filter(use) %>%
      select(-use) %>%
      mutate(Shape = st_intersection(Shape, .x$geometry), NUTS_ID=.x$NUTS_ID)
      ) %>%
    bind_rows()
  
}


corine_sf <- pblapply(codes[[1]], get_corine_sf) %>% bind_rows()

save(corine_sf, map, file="corine_sf.Rdata")

stop("Done")


theme_set(theme_light())
ggplot() +
  geom_sf(data=map) +
  geom_sf(data=corine_sf, mapping=aes(col=CODE_18))

