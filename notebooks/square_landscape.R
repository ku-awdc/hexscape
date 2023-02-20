library("hexscape")
library("tidyverse")
library("sf")

## Let's start with a square 50x50km landscape:
xrange <- c(0, 6)
yrange <- c(0, 7)
corners <- tribble(~x, ~y,
                   xrange[1], yrange[1],
                   xrange[2], yrange[1],
                   xrange[2], yrange[2],
                   xrange[1], yrange[2],
                   xrange[1], yrange[1]
)
landscape <- st_sfc(st_multipolygon(list(list(as.matrix(corners)))))
ggplot(landscape) + geom_sf()

## Add the hexagons:
patches_square <- generate_patches(landscape, hex_width=2)

ggplot(patches_square) + geom_sf()


## We could also make it more interesting:
p1 <- rbind(c(0,0), c(10,0), c(30,20), c(20,40), c(10,40), c(0,0))
p2 <- rbind(c(10,10), c(10,20), c(20,20), c(10,10))
landscape <- st_sfc(st_polygon(list(p1,p2)))
ggplot(landscape) + geom_sf()

patches_hole <- generate_patches(landscape, hex_width=8)

ggplot(patches_hole) + geom_sf()

## Calculate neighbours:
neighbours_square <- generate_neighbours(patches_square)
neighbours_hole <- generate_neighbours(patches_hole)

patches_square <- patches_square |>
  mutate(carrying_capacity = 10, pig_farms = rpois(n(), 1))  |>
  mutate(coords = asplit(st_coordinates(hex_centroid), 1)) |>
  unnest_wider(coords, "_")

patches_hole <- patches_hole |>
  mutate(carrying_capacity = 10, pig_farms = rpois(n(), 1))  |>
  mutate(coords = asplit(st_coordinates(hex_centroid), 1)) |>
  unnest_wider(coords, "_")

save(patches_square, patches_hole, neighbours_square, neighbours_hole, file="toy_patches.rda")
