# We should use the voroni thing to randomise farm locations

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

N <- 10
farms <- tibble(Farm=1:N, xloc=runif(N,xrange[1],xrange[2]), yloc=runif(N, yrange[1], yrange[2])) |>
  st_as_sf(coords=c("xloc","yloc"))

ggplot() +
  geom_sf(data=landscape) +
  geom_sf(data=st_voronoi(st_union(farms$geometry))) +
  geom_sf(data=farms) +
  coord_sf(xlim=xrange, ylim=yrange) +
  theme_void()

ggplot() +
  geom_sf(data=landscape) +
  geom_sf(data=st_convex_hull(st_union(farms$geometry))) +
  geom_sf(data=farms) +
  coord_sf(xlim=xrange, ylim=yrange) +
  theme_void()

