# /Library/Frameworks/R.framework/Resources/Rscript -e "source('denmark_estonia.R')"

### Steps to replicate hexagons (takes a while to run):

library("HexScape")

## 1) Set path to folder with raw spatial data and intermediate objects:

set_storage_folder("~/Documents/Resources/Datasets/HexScape")
# then put clc_legend.csv and u2018_clc2018_v2020_20u1_geoPackage into the raw_data folder

## 2) Process and cache corine data for Denmark:

# Downloads and caches high res map of DK:
map_dk <- extract_map("DK")
map_ee <- extract_map("EE")
# Processes raw corine data and saves processed_data/corine_DK.rds
land_use_dk <- extract_corine("DK", verbose=2L)
land_use_ee <- extract_corine("EE", verbose=2L)
# Note: the second time these are run they load cached data

## 3) Group and summarise corine data

clc <- extract_clc() %>%
  mutate(Category = case_when(
    CLC_LABEL2 == "Urban fabric" ~ "Impassable",
    CLC_LABEL3 == "Industrial or commercial units" ~ "Impassable",
    CLC_LABEL1 == "Artificial surfaces" ~ "Passable",
    CLC_CODE %in% c("243", "324", "322", "411", "421", "412", "321", "222") ~ "Medium",
    CLC_LABEL1 == "Water bodies" ~ "Passable",
    CLC_LABEL2 == "Open spaces with little or no vegetation" ~ "Passable",
    CLC_CODE %in% c("243","244") ~ "High",
    CLC_LABEL1 == "Agricultural areas" ~ "Low",
    CLC_LABEL2 == "Forests" ~ "High",
    CLC_LABEL3 == "Transitional woodland-shrub" ~ "High",
    CLC_LABEL2 == "Scrub and/or herbaceous vegetation associations" ~ "Low",
    CLC_LABEL1 == "Wetlands" ~ "Low",
    CLC_LABEL1 == "Unknown" ~ "Passable",
    TRUE ~ NA_character_
  )) %>%
  mutate(Category = ordered(Category, levels=c("Impassable","Passable","Low","Medium","High"))) %>%
  arrange(Category, CLC_CODE)
stopifnot(all(!is.na(clc$Category)))

land_use_denmark <- land_use_dk %>%
  left_join(clc %>% select(CLC_CODE, Category), by="CLC_CODE") %>%
  filter(CLC_LABEL1 != "Unknown") %>%
  group_by(Category) %>%
  summarise(AREA_HA = sum(AREA_HA), Shape = sf::st_union(Shape), .groups="drop")

land_use_estonia <- land_use_ee %>%
  left_join(clc %>% select(CLC_CODE, Category), by="CLC_CODE") %>%
  filter(CLC_LABEL1 != "Unknown") %>%
  group_by(Category) %>%
  summarise(AREA_HA = sum(AREA_HA), Shape = sf::st_union(Shape), .groups="drop")

# Simplify land use boundaries for computational reasons:
land_use_denmark <- land_use_denmark %>%
  rmapshaper::ms_simplify(keep=0.25, keep_shapes=TRUE, explode=TRUE, method="dp")
land_use_estonia <- land_use_estonia %>%
  rmapshaper::ms_simplify(keep=0.25, keep_shapes=TRUE, explode=TRUE, method="dp")

## 3) Generate hexagon patches from this map:
patches_dk <- generate_patches(map_dk, hex_width=2000, land_use=land_use_denmark)
neighbours_dk <- generate_neighbours(patches_dk, calculate_border=TRUE)
save(patches_dk, neighbours_dk, map_dk, file="patches/patches_denmark.rda")

patches_ee <- generate_patches(map_ee, hex_width=2000, land_use=land_use_estonia)

## 4) Generate neighbours:

neighbours_ee <- generate_neighbours(patches_ee, calculate_border=TRUE)

save(patches_ee, neighbours_ee, map_ee, file="patches_estonia.rda")

stop()


neighbours <- neighbours %>%
  filter(!is.na(Index), !is.na(Neighbour))

## 5) Generate network representation (igraph) and pairwise distances

library("igraph")

graph <-
  graph_from_data_frame(
    neighbours_dk,
    directed = TRUE,
    vertices = patches_dk %>%
      as_tibble() %>%
      filter(!is.na(Index)) %>%  # Remove the fake index for impassable areas
      select(Index, centroid, hex_centroid, area, lu_sum, starts_with("LU"))
  )

distances <- shortest.paths(graph)
distances[1:10,1:10]


save(patches, neighbours, distances, map_jutland, file="south_jutland_patches.rda")

ggplot(patches_dk %>% filter(!is.na(Index)), aes(fill=LU_High*4 + LU_Medium*2 + LU_Low, col=LU_High*4 + LU_Medium*2 + LU_Low)) + geom_sf(lwd=0) + theme(legend.title = element_blank()) + geom_sf(fill="grey", col="grey", data=patches_dk %>% filter(is.na(Index)), lwd=0)

# TODO: cartesian angle between hex_centroids
ind_cent <- patches %>%
  filter(!is.na(Index)) %>%
  mutate(hex_centroid = st_transform(hex_centroid, "WGS84")) %>%
  # Extract Northing and Easting:
  mutate(Longitude = st_coordinates(hex_centroid)[,1], Latitude = st_coordinates(hex_centroid)[,2]) %>%
  as_tibble() %>%
  select(Index, Easting=Longitude, Northing=Latitude)

directions <- expand_grid(Index = ind_cent$Index, Neighbour = ind_cent$Index) %>%
  mutate(IEast = ind_cent$Easting[Index], INorth = ind_cent$Northing[Index]) %>%
  mutate(NEast = ind_cent$Easting[Neighbour], NNorth = ind_cent$Northing[Neighbour])


directions <- expand_grid(Index = ind_cent$Index, Neighbour = ind_cent$Index) %>%
  mutate(IEast = ind_cent$Easting[Index], INorth = ind_cent$Northing[Index]) %>%
  mutate(NEast = ind_cent$Easting[Neighbour], NNorth = ind_cent$Northing[Neighbour]) %>%
  mutate(dEast = IEast - NEast, dNorth = INorth - NNorth)


angle <- geosphere::bearing(directions[c("INorth","IEast")], directions[c("NNorth","NEast")])
r <- sample(nrow(directions),20)
plot(directions[["dEast"]][r], directions[["dNorth"]][r], col="white")
text(round(directions[["Angle"]][r]), x=directions[["dEast"]][r], y=directions[["dNorth"]][r])
points(0,0)
directions[r,]




directions2 <- expand_grid(Index = ind_cent$Index, Neighbour = ind_cent$Index) %>%
  mutate(IEast = ind_cent$Easting[Index], INorth = ind_cent$Northing[Index]) %>%
  mutate(NEast = ind_cent$Easting[Neighbour], NNorth = ind_cent$Northing[Neighbour]) %>%
  mutate(dEast = IEast - NEast, dNorth = INorth - NNorth) %>%

  # https://www.igismap.com/formula-to-find-bearing-or-heading-angle-between-two-points-latitude-longitude/
  mutate(X = cos(IEast) * sin(dNorth)) %>%
  mutate(Y = cos(NEast) * sin(IEast) - sin(NEast) * cos(IEast) * cos(dNorth)) %>%
  mutate(Angle = case_when(
    Index == Neighbour ~ NA_real_,
    TRUE ~ atan2(X, Y) * 180/pi
  ))

plot(directions2$Angle, angle)


# Doesn't work:
directions
r <- sample(nrow(directions),20)
plot(directions[["dEast"]][r], directions[["dNorth"]][r], col="white")
text(round(directions[["Angle"]][r]), x=directions[["dEast"]][r], y=directions[["dNorth"]][r])
points(0,0)
directions[r,]




library("tidyverse")

pdat <- patches %>%
  arrange(Index) %>%
  mutate(Distance = distances[sample(1:nrow(distances), 1), ])

ggplot(pdat, aes(fill=Distance)) +
  geom_sf()

# Nodes that are not connected have Distance Inf (e.g. on islands):
pdat %>% filter(Distance == Inf)


