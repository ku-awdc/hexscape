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
jutland_nuts <- c("DK032","DK041","DK042","DK050")
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

land_use_summary <- land_use %>%
  filter(NUTS_ID %in% jutland_nuts) %>%
  as_tibble() %>%
  left_join(clc %>% select(CLC_CODE, Category), by="CLC_CODE") %>%
  group_by(Category, CLC_CODE, CLC_LABEL1, CLC_LABEL2, CLC_LABEL3) %>%
  summarise(area = sum(as.numeric(sf::st_area(Shape))), areaHA=sum(AREA_HA), .groups="drop") %>%
  arrange(desc(area)) %>%
  mutate(prop = round(area / sum(area), 3)) %>%
  mutate(Category2 = Category)

land_use_jutland <- land_use %>%
  filter(NUTS_ID %in% jutland_nuts) %>%
  # filter(NUTS_ID %in% "DK032") %>%
  left_join(clc %>% select(CLC_CODE, Category), by="CLC_CODE") %>%
  filter(CLC_LABEL1 != "Unknown") %>%
  group_by(Category) %>%
  summarise(AREA_HA = sum(AREA_HA), Shape = sf::st_union(Shape), .groups="drop")

## This is too detailed:
ptf <- ggplot(land_use_jutland, aes(col=Category, fill=Category)) + geom_sf()
# Don't even try:
# pt
ptf + coord_sf(xlim=c(500000, 550000), ylim=c(6100000, 6150000), crs= 25832, datum=sf::st_crs(25832))
ptf + coord_sf(xlim=c(520000, 530000), ylim=c(6120000, 6130000), crs= 25832, datum=sf::st_crs(25832))


# Simplify land use boundaries for computational reasons:
land_use_jutland <- land_use_jutland %>%
  rmapshaper::ms_simplify(keep=0.25, keep_shapes=TRUE, explode=TRUE, method="dp")
pt <- ggplot(land_use_jutland, aes(col=Category, fill=Category)) + geom_sf()
pt + coord_sf(crs= 25832, datum=sf::st_crs(25832))
pt + coord_sf(xlim=c(500000, 550000), ylim=c(6100000, 6150000), crs= 25832, datum=sf::st_crs(25832))
pt + coord_sf(xlim=c(520000, 530000), ylim=c(6120000, 6130000), crs= 25832, datum=sf::st_crs(25832))

land_use_summary_simp <- land_use_jutland %>%
  as_tibble() %>%
  group_by(Category) %>%
  summarise(area = sum(as.numeric(sf::st_area(Shape))), areaHA=sum(AREA_HA), .groups="drop") %>%
  arrange(desc(area)) %>%
  mutate(prop = round(area / sum(area), 3)) %>%
  mutate(Category2 = Category)


## 3) Generate hexagon patches from this map:

# map_jutland <- map %>% filter(NUTS_ID %in% "DK032") # South Jutland only
map_jutland <- map %>% filter(NUTS_ID %in% jutland_nuts)
patches <- generate_patches(map_jutland, hex_width=2000, land_use=land_use_jutland)

# Nearly all patches are mostly food or habitat:
summary(patches$LU_Medium + patches$LU_High)

ggplot(patches, aes(fill=LU_Medium+LU_High)) +
  geom_sf(color=NA)
ggplot(patches, aes(fill=LU_High)) +
  geom_sf(colour=NA)

