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
ggplot(patches) + geom_sf()


## We could also make it more interesting:
p1 <- rbind(c(0,0), c(10,0), c(30,20), c(20,40), c(10,40), c(0,0))
p2 <- rbind(c(10,10), c(10,20), c(20,20), c(10,10))
landscape <- st_sfc(st_polygon(list(p1,p2)))
ggplot(landscape) + geom_sf()

patches <- generate_patches(landscape, hex_width=1)
ggplot(patches) + geom_sf()


## Calculate neighbours:
neighbours <- generate_neighbours(patches)
neighbours

## So if we are currently at Index number 431 then we have 3 neighbours but only 1 with a big border:
ggplot(patches) + aes(col=Index==431) + geom_sf()
neighbours %>% filter(Index==431)
