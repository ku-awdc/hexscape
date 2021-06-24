library("HexScape")
library("tidyverse")
library("sf")

## Let's start with a square 50x50km landscape:
xrange <- c(0, 50)
yrange <- c(0, 50)
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
patches <- generate_patches(landscape, hex_width=1)

## Calculate neighbours:

## We could also make it more interesting:
p1 <- rbind(c(0,0), c(10,0), c(30,20), c(20,40), c(10,40), c(0,0))
p2 <- rbind(c(10,10), c(10,20), c(20,20), c(10,10))
landscape <- st_sfc(st_cast(st_polygon(list(p1,p2)), "MULTIPOLYGON"))
ggplot(landscape) + geom_sf()

# But this fails with errors...
