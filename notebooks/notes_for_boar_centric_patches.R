### Ideas for how to do boar-centric patch generation

# 1. Get boar_per_km2 estimates per land usage type, where impassable is NA_real_
# 2. Merge land usage data with boar_per_km2 then simplify and union
# 3. Generate regular spaced points (representing 1/10 or 1/100 wild boar each) for each land usage type and mask using (2)
# 4. Run a 2D kernal density using e.g.:
    # ?MASS::kde2d with n chosen so that rasters are e.g. 100m2 (minimum wild boar space)
    # https://dosull.github.io/posts/2021-10-21-kde/
    # NOTE: this will be good output for visualisation
# 5. Extract rasterised density estimates, filter over a threshold (1 per 100m2 or similar ?)
# 6. Make (5) back into vectorised sf by generating squares using st_make_grid aligned to centres of filtered (5), then union and maybe pad and then simplify
# 7. Separate multipolygon (6) into individual polygons
# 8. For each polygon:
#       - Remove if area is below some threshold (1km2?)
#       - Then calculate number of self-sufficient patches by p = (area %/% max_area) +1
#       - Then put a large number of regularly spaced points in the area (or st_sample?)
#       - Then run a k-means clustering with k=p to get centroids
#       - Then do voronoi tesselation to break up area
#       - Then mask with area to get patches
# 9. Re-merge polygons with vectorised boar_per_km2 to get carrying capacities of habitable patches
# 10. For the non-habitable areas, either leave as they are (patches are either contiguous or need long-distance movement), or break them into regularly sized polygons of transit zones


# Note: st_make_grid can also do hexagons...!
# https://r-spatial.github.io/sf/reference/st_make_grid.html


# Note: someone else had a similar idea for (8):
if(FALSE){
  # https://gis.stackexchange.com/questions/375345/dividing-polygon-into-parts-which-have-equal-area-using-r

  library(sf)
  library(mapview)
  library(tidyverse)
  library(dismo)
  library(osmdata)
  library(mapview)

  split_poly <- function(sf_poly, n_areas){
    # create random points
    points_rnd <- st_sample(sf_poly, size = 10000)
    #k-means clustering
    points <- do.call(rbind, st_geometry(points_rnd)) %>%
      as_tibble() %>% setNames(c("lon","lat"))
    k_means <- kmeans(points, centers = n_areas)
    # create voronoi polygons
    voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
    # clip to sf_poly
    crs(voronoi_polys) <- crs(sf_poly)
    voronoi_sf <- st_as_sf(voronoi_polys)
    equal_areas <- st_intersection(voronoi_sf, sf_poly)
    equal_areas$area <- st_area(equal_areas)
    return(equal_areas)
  }

  pol <- osmdata::getbb("aguascalientes", format_out = "sf_polygon")
  pol_areas <- split_poly(pol, 20)
  mapview(pol_areas)
}

