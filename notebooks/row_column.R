#### Re-implement patches for row and column representation used by

library("sf")
library("tidyverse")

## Let's start with a square 50x50km landscape:
xrange <- c(0, 10) + runif(1,-50,50)
yrange <- c(0, 10) + runif(1,-50,50)
corners <- tribble(~x, ~y,
                   xrange[1], yrange[1],
                   xrange[2], yrange[1],
                   xrange[2], yrange[2],
                   xrange[1], yrange[2],
                   xrange[1], yrange[1]
)
landscape <- st_sfc(st_multipolygon(list(list(as.matrix(corners)))))

centerpoint <- st_centroid(landscape)
reference_point <- st_point(runif(2,-10,20))

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
yrange <- as.numeric(st_bbox(reference_point)["ymin"] - st_bbox(landscape)[c("ymin","ymax")]) / ((hexlth+hexhgt)/2)
r <- seq(floor(yrange[2]-1), ceiling(yrange[1]+1))
r <- seq(floor(yrange[2]), ceiling(yrange[1]))
yrange
r

## For column numbers (q) we need to distort the boundary box relative to the reference point:
refy <- st_bbox(reference_point)["ymin"]
refx <- st_bbox(reference_point)["xmin"]
bbox <- st_bbox(landscape)

bbox["xmax"] + (bbox["ymax"] - refy) * tanpi(1/6)
bbox["xmin"] + (bbox["ymin"] - refy) * tanpi(1/6)

xrange <- (c(
  as.numeric(bbox["xmin"] + (bbox["ymin"] - refy) * tanpi(1/6)),
  as.numeric(bbox["xmax"] + (bbox["ymax"] - refy) * tanpi(1/6))
) - refx) / hexwth

q <- seq(floor(xrange[1]-1), ceiling(xrange[2]+1))
q <- seq(floor(xrange[1]), ceiling(xrange[2]))
q

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
  mutate(xx=x, yy=y) %>%
  st_as_sf(coords=c("xx","yy")) %>%
  mutate(dist = st_distance(geometry, landscape)[,1], keep=dist < (hexhgt/2)) %>%
  as_tibble() %>%
  split(.$index) %>%
  lapply(function(.x) .x %>% mutate(geometry = genpoly(x, y))) %>%
  bind_rows()

pt1 <- ggplot() +
  geom_sf(aes(geometry=geometry, fill=keep), hexagons) +
  geom_sf(aes(geometry=geometry), landscape) +
  theme(legend.pos="none")

hexagons <- hexagons %>%
  mutate(OK = st_intersects(geometry, landscape, sparse=FALSE)[,1]) %>%
  filter(OK) %>%
  select(-OK) %>%
  st_as_sf() %>%
  st_intersection(landscape) %>%
  mutate(area = as.numeric(st_area(geometry), units="m")) %>%
  filter(area >= min_prop * hexarea)


pt2 <- ggplot(hexagons, aes(x=x, y=y, label=lab, geometry=geometry)) +
  geom_sf() +
  geom_text()

ggpubr::ggarrange(pt1, pt2)


patches <- generate_patches(landscape, hexwth, reference_point)
ggplot(patches) +
  geom_sf(aes(geometry=geometry)) +
  geom_sf_text(aes(geometry=centroid, label=str_c(r, ", ", q)))
