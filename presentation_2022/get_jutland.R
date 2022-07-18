library('tidyverse')
library('HexScape')

## 1) Set path to folder with raw spatial data and intermediate objects:

set_storage_folder("~/Documents/Resources/Datasets/HexScape")
# then put clc_legend.csv and u2018_clc2018_v2020_20u1_geoPackage into the raw_data folder

## 2) Process and cache corine data for Denmark:

# Downloads and caches high res map of DK:
map_dk <- extract_map("DK")
# Processes raw corine data and saves processed_data/corine_DK.rds
land_use_dk <- extract_corine("DK", verbose=2L)
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

# Simplify land use boundaries for computational reasons:
#land_use_denmark <- land_use_denmark %>%
#  rmapshaper::ms_simplify(keep=0.25, keep_shapes=TRUE, explode=TRUE, method="dp")

## 3) Generate hexagon patches from this map:

map <- map_dk %>%
  filter(NUTS_ID %in% c("DK032","DK041","DK050","DK042")) %>%
  filter(NUTS_ID %in% "DK032")

# ggplot(map) + geom_sf()

patches <- generate_patches(map, hex_width=2000, land_use=land_use_denmark, name="DK032", reference_point = sf::st_point(c(0,0)))

# Add carrying capacity:
patches <- patches %>%
  mutate(carrying_proportion = 1*LU_High + 0.5*LU_Medium + 0.1*LU_Low) %>%
  mutate(carrying_capacity = (25*carrying_proportion*area) / 3464102)
# Max is 25 carrying_capacity for 3.5km2 at high LU suitability

# Add number of pig farms:
library(goldfinger)
library(sf)
(gy_load("/Users/matthewdenwood/Documents/Research/Papers/Hexscape paper/pig_locations.rdg"))

pigs %>% count(brugsart)
pigs_using <- pigs %>%
  filter((besstr_1501+besstr_1502+besstr_1504)>0L) %>%
  select(brugsart, x = staldkoordinat.x_koordinat, y = staldkoordinat.y_koordinat) %>%
  st_as_sf(coords=c("x","y"), crs=st_crs(25832))  %>% # https://epsg.io/25832
  st_transform(st_crs(map)) %>%
  mutate(using = st_intersects(geometry, map, sparse=FALSE)[,1]) %>%
  filter(using)

pigs_using %>% as_tibble() %>% count(brugsart)

mtch <- st_nearest_feature(pigs_using$geometry, patches$geometry)
stopifnot(length(mtch)==nrow(pigs_using))
pigs_using$Index <- patches[mtch,]$Index

patches <- patches %>%
  left_join( pigs_using %>% as_tibble() %>% count(Index, name="pig_farms"), by="Index") %>%
  replace_na(list(pig_farms=0L))

patches %>% as_tibble() %>% count(pig_farms)
stopifnot(sum(patches$pig_farms) == nrow(pigs_using))

ggplot() +
  geom_sf(data=map, aes(geometry=geometry), fill="dark grey") +
  geom_sf(data=patches, aes(geometry=geometry, fill=carrying_capacity, col=carrying_capacity)) +
  geom_sf(data=pigs_using, aes(geometry=geometry), col="red", alpha=0.25, size=0.5) +
  theme_void()
ggsave("/Users/matthewdenwood/Documents/Research/Papers/Hexscape paper/DK032_with_pig_farms.pdf")

ggplot() +
  geom_sf(data=map, aes(geometry=geometry), fill="dark grey") +
  geom_sf(data=patches, aes(geometry=geometry, fill=carrying_capacity, col=carrying_capacity)) +
  theme_void()
ggsave("/Users/matthewdenwood/Documents/Research/Papers/Hexscape paper/DK032_cc.pdf")

ggplot() +
  geom_sf(data=map, aes(geometry=geometry), fill="dark grey") +
  geom_sf(data=patches, aes(geometry=geometry, fill=pig_farms, col=pig_farms)) +
  theme_void()
ggsave("/Users/matthewdenwood/Documents/Research/Papers/Hexscape paper/DK032_pf.pdf")

neighbours <- generate_neighbours(patches, calculate_border=TRUE)

## 4) Generate neighbours:


patches %>%
  mutate(coords = asplit(st_coordinates(hex_centroid), 1)) %>%
  unnest_wider(coords, "_") ->
patches

save(patches, neighbours, map, file="/Users/matthewdenwood/Documents/Research/Papers/Hexscape paper/patches_DK032.rda")

(load("/Users/matthewdenwood/Documents/Research/Papers/Hexscape paper/patches_DK032.rda"))
