library("HexScape")
library("tidyverse")
library("sf")

set.seed(20210629)

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
#'
#'
#'
## Calculate neighbours:
neighbours <- generate_neighbours(patches)
neighbours
#'
#'
#' ### Add Carrying Capacity
#'
#'
patches %>%
  mutate(carrying_capacity = runif(n(), 7, 48) %>%
           round() %>%
           as.integer()) ->
  patches
#'
patches %>%
  as_tibble() %>%
  mutate(hex_centroid = st_coordinates(hex_centroid) %>% as_tibble()) %>%
  unpack(hex_centroid, names_sep = "_") %>%
  mutate(centroid = centroid %>% st_coordinates() %>% as_tibble()) %>%
  unpack(centroid, names_sep = "_") %>%
  select(-geometry) %>%
  write_delim("../rust_asf/dataset/patches_square.csv", delim = ";")

neighbours %>%
  write_delim("../rust_asf/dataset/neighbours_square.csv", delim = ";")
#'

square_patches <- patches
square_neighbours <- neighbours

save(square_patches, square_neighbours, file = "../rust_asf/dataset/square_spatial_map.rds",
     compress = FALSE)

#'
#'
## We could also make it more interesting:
p1 <- rbind(c(0,0), c(10,0), c(30,20), c(20,40), c(10,40), c(0,0))
p2 <- rbind(c(10,10), c(10,20), c(20,20), c(10,10))
landscape <- st_sfc(st_polygon(list(p1,p2)))
ggplot(landscape) + geom_sf()

patches <- generate_patches(landscape, hex_width=1)
ggplot(patches) + geom_sf()

#'
#'
patches %>%
  mutate(carrying_capacity = runif(n(), 7, 48) %>%
           round() %>%
           as.integer()) ->
  patches
#'
#'
#'
ggplot(patches) +
  geom_sf(aes(fill = factor(carrying_capacity)),
          show.legend = FALSE) +
  scale_fill_viridis_d()
#'
#'
#'


## Calculate neighbours:
neighbours <- generate_neighbours(patches)
neighbours
#'
#'
#'

patches %>%
  as_tibble() %>%
  mutate(hex_centroid = st_coordinates(hex_centroid) %>% as_tibble()) %>%
  unpack(hex_centroid, names_sep = "_") %>%
  mutate(centroid = centroid %>% st_coordinates() %>% as_tibble()) %>%
  unpack(centroid, names_sep = "_") %>%
  select(-geometry) %>%
  write_delim("../rust_asf/dataset/patches_square_punctured.csv", delim = ";")

neighbours %>%
  write_delim("../rust_asf/dataset/neighbours_square_punctured.csv", delim = ";")
#'
#'


punctured_patches <- patches
punctured_neighbours <- neighbours

save(punctured_patches, punctured_neighbours,
     file = "../rust_asf/dataset/punctured_spatial_map.rds",
     compress = FALSE)
#'
## So if we are currently at Index number 431 then we have 3 neighbours but only 1 with a big border:
ggplot(patches) + aes(col=Index==431, fill=Index==431) + geom_sf()
neighbours %>% filter(Index==431)
#'
#'
#'
#'
