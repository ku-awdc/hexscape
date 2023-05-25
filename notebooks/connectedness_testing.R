library("sf")
library("hexscape")

xrange <- c(0, 50)
yrange <- c(0, 50)
corners <- tribble(~x, ~y,
                   xrange[1], yrange[1],
                   xrange[2], yrange[1],
                   xrange[2], yrange[2],
                   xrange[1], yrange[2],
                   xrange[1], yrange[1]
)
landscape <- st_multipolygon(list(list(as.matrix(corners)))) |> st_sfc() |> st_as_sf()
farms <- tibble(Index = 1:100L) |> mutate(geometry = st_sample(landscape, n()) |> st_sfc()) |> st_as_sf()
patches <- discretise_voronoi(landscape, farms)

connectedness_fun <- function(x) 0.5 * 1/x
max_distance <- 5

connectedness <- generate_connectedness(patches, connectedness_fun, max_distance, grid_resolution=50, sparse=FALSE)

c1 <- connectedness
