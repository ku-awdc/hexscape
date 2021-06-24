#'
#'
#'
#'
library(tidyverse)
library(magrittr)
library(sf)
library(tmap)
options(device = "windows")
#'
#'
#'
load("patches/patches_estonia.rda")
#'
#'
#'
ls()
map_ee %>% glimpse()
neighbours_ee %>% glimpse()
patches_ee %>% glimpse()
#''
#'
ggplot( patches_ee %>% na.omit(Index)) +
  geom_sf(aes(fill = LU_High))
#'
#'
#'
tmap_options(check.and.fix = TRUE)
tmap_mode("view")
#'
#'
#'
tmap::tm_shape(patches_ee %>% na.omit(Index) %>% filter(Index == 1)) +
  tm_polygons() +
  tm_layout()
