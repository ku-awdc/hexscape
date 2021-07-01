#'
library("HexScape")
library("tidyverse")
library("sf")
set.seed(20210629)
source("../rust_asf/rscripts/eda_startup.r")
#'
load("patches/patches_denmark.rda")
#'
#'
map_dk
neighbours_dk
patches_dk <- patches_dk %>%
  filter(!is.na(Index))
#'
#'
#'
#' There are patches that are less area than the ideal.
maximal_cc <- 35 / patches_dk %>%
  drop_na(Index) %>%
  pull("area") %>%
  max()
#'
#'
#'
patches_dk <- patches_dk %>%
  mutate(carrying_capacity_relative = LU_High * maximal_cc +
           LU_Medium * maximal_cc / 2 + LU_Low * maximal_cc / 4,
         carrying_capacity = carrying_capacity_relative * area,
         carrying_capacity = as.integer(round(carrying_capacity)))
#'
patches_dk$carrying_capacity
#'
#'
patches_dk %>%
  as_tibble() %>%
  mutate(hex_centroid = st_coordinates(hex_centroid) %>% as_tibble()) %>%
  unpack(hex_centroid, names_sep = "_") %>%
  mutate(centroid = centroid %>% st_coordinates() %>% as_tibble()) %>%
  unpack(centroid, names_sep = "_") %>%
  select(-geometry) %>%
  write_delim("../rust_asf/dataset/patches_dk.csv", delim = ";")
#'
#'
neighbours_dk %>%
  write_delim("../rust_asf/dataset/neighbours_dk.csv", delim = ";")
#'
#'
#'
#'
#'
library(tmap)
tmap_mode("view")
patches_dk$carrying_capacity[384]
tmap_options(check.and.fix = TRUE)
tm_shape(patches_dk) +
  tm_polygons("carrying_capacity")
#'
#'
#'
#'
#'
#'
stop("Finished script");
#'
#' ## Compare to previous approach
#'
#'
#'
patches_dk$cc %>%
  hist()
#'
#'
load("../asf_phd/WBModel/data/WBMat.rda")
#'
#'
WBMat$region_type
