# /Library/Frameworks/R.framework/Resources/Rscript -e "source('denmark_estonia.R')"

### Steps to replicate hexagons (takes a while to run):

library("HexScape")

## 1) Set path to folder with raw spatial data and intermediate objects:

set_storage_folder("~/Documents/Resources/Datasets/HexScape")
# then put clc_legend.csv and u2018_clc2018_v2020_20u1_geoPackage into the raw_data folder

## 2) Process and cache corine data for Denmark:

# Downloads and caches high res map of DK:
map <- extract_map("DK")
# Processes raw corine data and saves processed_data/corine_DK.rds
land_use <- extract_corine("DK", verbose=2L)
# Note: the second time these are run they load cached data

# Plot of land use types in Jutland:
jutland_nuts <- c("DK032","DK041","DK050")
theme_set(theme_light())
ggplot() +
  geom_sf(data=map %>% filter(NUTS_ID %in% jutland_nuts)) +
  geom_sf(data=land_use %>% filter(NUTS_ID %in% jutland_nuts), mapping=aes(fill=CLC_LABEL1, col=CLC_LABEL1))

## 3) Group and summarise corine data for Jutland

clc <- extract_clc() %>%
  mutate(Category = case_when(
    CLC_LABEL2 == "Urban fabric" ~ "Impassable",
    CLC_LABEL3 == "Industrial or commercial units" ~ "Impassable",
    CLC_LABEL1 == "Artificial surfaces" ~ "Passable",
    CLC_LABEL1 == "Water bodies" ~ "Passable",
    CLC_LABEL2 == "Open spaces with little or no vegetation" ~ "Passable",
    CLC_CODE %in% c("243","244") ~ "Habitat",
    CLC_LABEL1 == "Agricultural areas" ~ "Food",
    CLC_LABEL2 == "Forests" ~ "Habitat",
    CLC_LABEL3 == "Transitional woodland-shrub" ~ "Habitat",
    CLC_LABEL2 == "Scrub and/or herbaceous vegetation associations" ~ "Food",
    CLC_LABEL1 == "Wetlands" ~ "Food",
    CLC_LABEL1 == "Unknown" ~ "Passable",
    TRUE ~ NA_character_
  ))
stopifnot(all(!is.na(clc$Category)))

land_use_summary <- land_use %>%
  filter(NUTS_ID %in% jutland_nuts) %>%
  as_tibble() %>%
  left_join(clc %>% select(CLC_CODE, Category), by="CLC_CODE") %>%
  group_by(Category, CLC_CODE, CLC_LABEL1, CLC_LABEL2, CLC_LABEL3) %>%
  summarise(area = sum(as.numeric(st_area(Shape))), areaHA=sum(AREA_HA), .groups="drop") %>%
  arrange(desc(area)) %>%
  mutate(prop = round(area / sum(area), 3)) %>%
  mutate(Category2 = Category)

land_use_jutland <- land_use %>%
  filter(NUTS_ID %in% jutland_nuts) %>%
  # filter(NUTS_ID %in% "DK032") %>%
  left_join(clc %>% select(CLC_CODE, Category), by="CLC_CODE") %>%
  filter(CLC_LABEL1 != "Unknown") %>%
  group_by(Category) %>%
  summarise(AREA_HA = sum(AREA_HA), Shape = st_union(Shape), .groups="drop") %>%
  mutate(Category = factor(Category, levels=c("Impassable","Passable","Food","Habitat")))

ggplot(land_use_jutland, aes(col=Category, fill=Category)) + geom_sf()

# Simplify land use boundaries for computational reasons:
land_use_jutland <- land_use_jutland %>%
  rmapshaper::ms_simplify(keep=0.05, keep_shapes=FALSE)
ggplot(land_use_jutland, aes(col=Category, fill=Category)) + geom_sf()


## 3) Generate hexagon patches from this map:

# map_jutland <- map %>% filter(NUTS_ID %in% "DK032") # South Jutland only
map_jutland <- map %>% filter(NUTS_ID %in% jutland_nuts)
patches <- generate_patches(map_jutland, hex_width=2000, land_use=land_use_jutland)

# Nearly all patches are mostly food or habitat:
summary(patches$Food + patches$Habitat)

ggplot(patches, aes(fill=Food+Habitat)) +
  geom_sf()
ggplot(patches, aes(fill=Habitat)) +
  geom_sf()
ggplot(patches, aes(fill=BoarScore)) +
  geom_sf()


ggplot(patches) + geom_sf()
