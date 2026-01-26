library("tidyverse")
library("sf")

(load("hexscape_extract.rda"))

map_dk |>
  st_bbox() |>
  st_make_grid(n = c(50, 50)) |>
  st_intersection(map_dk |> st_union()) |>
  st_as_sf() |>
  ggplot() +
  geom_sf() +
  theme_void()
