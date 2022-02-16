#### Re-implement patches for row and column representation used by

library("sf")
library("tidyverse")

## Let's start with a square 50x50km landscape:
xrange <- c(0, 10)
yrange <- c(0, 10)
corners <- tribble(~x, ~y,
                   xrange[1], yrange[1],
                   xrange[2], yrange[1],
                   xrange[2], yrange[2],
                   xrange[1], yrange[2],
                   xrange[1], yrange[1]
)
landscape <- st_sfc(st_multipolygon(list(list(as.matrix(corners)))))

centerpoint <- st_centroid(landscape)
reference_point <- st_point(st_bbox(landscape)[c("xmin","ymax")])

hexwth <- 2
# Height (corner to corner):
hexhgt <- 2*hexwth / 3^0.5
# Side length:
hexlth <- hexhgt/2
# Max area:
hexarea <- sqrt(3)*hexwth^2/2

min_prop <- 0.01

## We will use an axial coordinate system for hexagons:
# https://www.redblobgames.com/grids/hexagons/#map-storage

## Row numbers (r) are relatively easy:
yrange <- as.numeric(st_bbox(reference_point)["ymin"] - st_bbox(landscape)[c("ymin","ymax")]) / hexwth
r <- seq(floor(yrange[2]-1), ceiling(yrange[1]+1))

## Column numbers (q) are a bit harder as the indexes increase bottom left to top right:
bl <- - as.numeric(st_distance(reference_point, st_point(st_bbox(landscape)[c("xmin","ymin")]))) / hexwth
tr <- as.numeric(st_distance(reference_point, st_point(st_bbox(landscape)[c("xmax","ymax")]))) / hexwth
q <- seq(ceiling(tr), floor(bl))

genpoly <- function(x, y){
  hexpoints <- t(matrix(c(
    0, hexhgt/2,
    hexwth/2, hexlth/2,
    hexwth/2, -hexlth/2,
    0, -hexhgt/2,
    -hexwth/2, -hexlth/2,
    -hexwth/2, hexlth/2,
    0, hexhgt/2
  ), nrow=2) + c(x,y))
  st_sfc(st_polygon(list(hexpoints)), crs=st_crs(landscape))
}

hexagons <- expand_grid(r=r, q=q) %>%
  mutate(y = st_bbox(reference_point)["ymin"] - r*(hexlth+hexhgt)/2) %>%
  mutate(x = st_bbox(reference_point)["xmin"] + r*hexwth/2 + q*hexwth) %>%
  mutate(index=1:n(), lab = str_c(q, ", ", r)) %>%
  ## TODO: use st_distance to landscape to filter anything where point > hexwidth away
  split(.$index) %>%
  lapply(function(.x) .x %>% mutate(geometry = genpoly(x, y))) %>%
  bind_rows() %>%
  mutate(OK = st_intersects(geometry, landscape, sparse=FALSE)[,1]) %>%
  filter(OK) %>%
  select(-OK) %>%
  st_as_sf() %>%
  st_intersection(landscape) %>%
  mutate(area = as.numeric(st_area(geometry), units="m")) %>%
  filter(area >= min_prop * hexarea)


ggplot(hexagons, aes(x=x, y=y, label=lab, geometry=geometry)) +
  geom_sf() +
  geom_text()

