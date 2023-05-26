library("sf")
library("hexscape")

xrange <- c(0, 10)
yrange <- c(0, 10)
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

connectedness_fun <- function(x) 1/x
dd <- seq(0.1,10,by=0.1)
plot(dd, connectedness_fun(dd), type="l"); abline(h=0, lty="dashed")
max_distance <- 5

connectedness <- generate_connectedness(patches, connectedness_fun, max_distance, grid_resolution=50, sparse=FALSE)
plot(connectedness, t(connectedness)); abline(0,1)

c1 <- generate_connectedness(patches, connectedness_fun, max_distance, grid_resolution=50, sparse=FALSE)
c2 <- generate_connectedness(patches, connectedness_fun, max_distance, grid_resolution=20, sparse=FALSE)
## Pretty good agreement (which increases for 50 vs 100 etc):
plot(c1, c2); abline(0,1)

farms <- tibble(Index = 1:250L) |> mutate(geometry = st_sample(landscape, n()) |> st_sfc()) |> st_as_sf()
patches <- discretise_voronoi(landscape, farms)
c3 <- generate_connectedness(patches, connectedness_fun, max_distance, grid_resolution=50, sparse=FALSE)
c4 <- generate_connectedness(patches, connectedness_fun, max_distance, grid_resolution=20, sparse=FALSE)

## Average total connectedness per patch is independent of number of patches:
sum(c1)/nrow(c1)
sum(c2)/nrow(c2)
sum(c3)/nrow(c3)
sum(c4)/nrow(c4)

## Notes:
# Run time is approximately linear in #patches, but not linear in grid_resolution
# max_distance and the connectedness_fun also impact run time
