## Code for centering hexagons in a landscape

library("tidyverse")
library("sf")

landscape <- st_sfc(st_polygon(list(rbind(c(0,0), c(40,0), c(40,40), c(0,40), c(0,0)))))
ggplot(landscape) + geom_sf()

## Hexagon dimensions:
hex_side <- runif(1, 1, 10)
hex_area <- 3/2 * sqrt(3) * hex_side
hex_diagonal <- 2 * hex_side
hex_width <- sqrt(3) * hex_side
hex_height <- hex_side * 2

## Center of landscape:
bb <- st_bbox(landscape)
ls_width <- as.numeric(bb["xmax"]-bb["xmin"])
ls_height <- as.numeric(bb["ymax"]-bb["ymin"])
center <- c(x=ls_width/2 + as.numeric(bb["xmin"]), y=ls_height/2 + as.numeric(bb["ymin"]))
# Or:
center <- st_centroid(landscape) |> st_coordinates() |> as.numeric() |> set_names(c("x","y"))

bb[] <- bb[] + c(-hex_width*2, -(hex_height+hex_side), hex_width*2, hex_height+hex_side)

## Calculating x offset is relatively easy:
offset_x <- (ls_width %% hex_width) / 2 + (hex_width/2)

## Calculating y offset is a bit trickier:
offset_y <- (ls_height/2+hex_height+hex_side) %% ((hex_height + hex_side) / 2)


#if(offset_y > ((hex_height + hex_side) / 4)){
#  offset_y <- offset_y - ((hex_height + hex_side) / 2)
#}

patches <- st_make_grid(st_as_sfc(bb), cellsize=hex_width, offset=c(offset_x, offset_y), square=FALSE) |> st_intersection(landscape) |> st_as_sf()

ggplot(patches) + geom_sf() + geom_point(aes(x=x,y=y), tibble(x=center["x"], y=center["y"]))


## Point can be in 1 of 4 places:
# - Center hexagon (correct)
# - Center side (correct I guess)
# - In line with bottom/top hips (incorrect - off by (hex_side/2))
# - At 3-way joint (incorrect - off by (hex_side/2))

## But correcting by hex_side/2 doesn't always fix the problem e.g. with hex_side <- 7.5:

patches <- st_make_grid(st_as_sfc(bb), cellsize=hex_width, offset=c(offset_x, offset_y+(hex_side/2)), square=FALSE) |> st_intersection(landscape) |> st_as_sf()

ggplot(patches) + geom_sf() + geom_point(aes(x=x,y=y), tibble(x=center["x"], y=center["y"]))




## Graveyard

st_make_grid(st_buffer(landscape, hex_width*2), cellsize=hex_width, offset=c(0.0, 0.0), square=FALSE, what="centers") |>
  st_coordinates() |>
  as_tibble() |>
  mutate(Xdelta = center["x"] - X, Ydelta = center["y"] - Y) |>
  filter(Xdelta > 0) |>
  arrange(abs(Ydelta), abs(Xdelta)) |>
  slice(1) |>
  select(Xdelta, Ydelta) |>
  as.numeric() ->
  offsets_i1

offsets_i1
offset_x

st_make_grid(st_buffer(landscape, hex_width*2), cellsize=hex_width, offset=offsets_i1, square=FALSE, what="centers") |>
  st_coordinates() |>
  as_tibble() |>
  mutate(Xdelta = center["x"] - X, Ydelta = center["y"] - Y) |>
  filter(Xdelta >= 0.0, Ydelta >= 0.0) |>
  arrange(Ydelta, Xdelta) |>
  slice(1) |>
  select(Xdelta, Ydelta) |>
  as.numeric() ->
  offsets_i2

offsets <- offsets_i1 + offsets_i2#c(0, offsets_i2[2])

patches <- st_make_grid(st_buffer(landscape, hex_width*2), cellsize=hex_width, offset=c(offset_x, 0), square=FALSE) |> st_intersection(landscape)
ggplot(patches) + geom_sf() + geom_point(aes(x=x,y=y), tibble(x=center["x"], y=center["y"]))

ggplot() + geom_sf(data=p1, col="blue") + geom_sf(data=p2, col="red") + geom_point(aes(x=x,y=y), tibble(x=center["x"], y=center["y"]))


 dx <- 1155  # Make dy as close to an integer as possible
# https://www.gigacalculator.com/calculators/hexagon-calculator.php
dy <- (3/2)*(dx/sqrt(3))
bb[c(1,2)] <- floor(bb[c(1,2)]/c(dx,dy))*c(dx,dy)
bb[c(3,4)] <- ceiling(bb[c(3,4)]/c(dx,dy))*c(dx,dy)
ns <- round(c((bb["xmax"]-bb["xmin"])/dx +1,(bb["ymax"]-bb["ymin"])/dy +1))
cellsize <- dx
offset <- c(bb["xmin"], bb["ymin"])
is_square <- FALSE

