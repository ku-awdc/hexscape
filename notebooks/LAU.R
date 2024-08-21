library("tidyverse")
library("sf")

lau <- st_read("~/Downloads/ref-lau-2021-01m.shp/LAU_RG_01M_2021_3035.shp/")

lau |>
  filter(CNTR_CODE=="DK")

nuts <- st_read("~/Downloads/NUTS_RG_01M_2021_3035.shp-2/")

full_join(
  nuts |>
    as_tibble() |>
    count(CNTR_CODE, name="nuts"),
  lau |>
    as_tibble() |>
    count(CNTR_CODE, name="lau"),
) |>
  print(n=Inf)


