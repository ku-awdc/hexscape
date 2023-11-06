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

patches <- st_make_grid(st_as_sfc(bb), cellsize=hex_width, offset=c(offset_x, offset_y), square=FALSE)
ggplot(patches |> st_intersection(landscape) |> st_as_sf()) + geom_sf() + geom_point(aes(x=x,y=y), tibble(x=center["x"], y=center["y"]))

## But point can be in 1 of 4 places:
# - Center hexagon (correct)
# - Center side (correct I guess)
# - In line with bottom/top hips (incorrect - off by (hex_side/2))
# - At 3-way joint (incorrect - off by (hex_side/2))
## And correcting by hex_side/2 doesn't always fix the problem!


## But we can fix it manually:
ggplot(patches |> st_intersection(landscape) |> st_as_sf()) + geom_sf() + geom_point(aes(x=x,y=y), tibble(x=center["x"], y=center["y"]))

patches |>
  st_centroid() |>
  st_coordinates() |>
  as_tibble() |>
  mutate(Xdelta = center["x"] - X, Ydelta = center["y"] - Y) |>
  arrange(abs(Xdelta), abs(Ydelta)) |>
  slice(1) |>
  select(Xdelta, Ydelta) |>
  as.numeric() ->
  offsets

## And then choose either the center of a hexagon:
(patches + offsets) |>
  st_intersection(landscape) |>
  ggplot() + geom_sf() + geom_point(aes(x=x,y=y), tibble(x=center["x"], y=center["y"]))

## Or the center of a side:
(patches + offsets + c(hex_width/2, 0)) |>
  st_intersection(landscape) |>
  ggplot() + geom_sf() + geom_point(aes(x=x,y=y), tibble(x=center["x"], y=center["y"]))


## Then we can add something akin to row and column numbers:
side_y <- st_linestring(matrix(bb[c("xmin","xmin","ymin","ymax")], ncol=2))
side_x <- st_linestring(matrix(bb[c("xmin","xmax","ymin","ymin")], ncol=2))
# Note: this bb is deliberately with the inflated bb!

(patches + offsets) |>
  st_as_sf() |>
  rename(geometry=x) |>
  mutate(Centroid = st_centroid(geometry)) |>
  filter(st_intersects(geometry, landscape, sparse=FALSE)[,1]) |>
  mutate(geometry = st_intersection(geometry, landscape)) |>
  mutate(Col = st_distance(Centroid, side_x) |> factor() |> as.numeric()) |>
  mutate(Row = st_distance(Centroid, side_y) |> factor() |> as.numeric()) ->
  final_patches

ggplot(final_patches, aes(label=Col)) + geom_sf() + geom_sf_label()
ggplot(final_patches, aes(label=Row)) + geom_sf() + geom_sf_label()

# Then you can just change the row/column order as you prefer:
final_patches |>
  arrange(Row, Col) |>
  mutate(PatchID = row_number()) |>
  select(PatchID, everything()) ->
  final_patches

final_patches




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

