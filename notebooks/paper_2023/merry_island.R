### Investigate Merry Island data

library("HexScape")
library("tidyverse")
library("sf")

island <- st_read("/Users/matthewdenwood/Documents/Students/Graduate Students/Mossa Merhi Reimert/Merry Island/initial_data/Island_ADMIN.dbf")

ggplot(island) + geom_sf()

library("eurostat")

eu <- get_eurostat_geospatial()
ggplot(eu |> filter(CNTR_CODE=="FR", LEVL_CODE==1, !FID %in% c("FR1","FRM","FRY"))) +
  geom_sf(mapping=aes(fill=FID)) +
  geom_sf(data=island, alpha=0.2, col="red")

ggplot(eu |> filter(CNTR_CODE=="FR", FID %in% c("FRJ","FRK")), aes(fill=FID)) + geom_sf()
ggplot(island) + geom_sf()

###


set_storage_folder("~/Documents/Resources/Datasets/HexScape")
fr <- extract_map("FR")
land_use <- extract_corine("FR", verbose=2L)

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


mi_land_use <- land_use |> filter(str_detect(NUTS_ID, "^FRJ.*") | str_detect(NUTS_ID, "^FRK.*"))

ggplot(mi_land_use, aes(fill=CLC_LABEL1)) + geom_sf()
ggsave("merry_island.pdf")

mi_land_use |>
  group_by(CLC_CODE, CLC_LABEL1, CLC_LABEL2, CLC_LABEL3) |>
  summarise(Shape = st_union(Shape), AREA_HA = sum(AREA_HA), .groups="drop") ->
  mi_agg

mi_agg |> as_tibble() |> select(-Shape) |> arrange(desc(AREA_HA)) -> mi_tot

ggplot(mi_agg, aes(fill=CLC_LABEL2)) + geom_sf()
ggsave("merry_island.pdf")
