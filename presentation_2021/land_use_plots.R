## Code to make plots and animations for Mossa's presentation July 2021

library("HexScape")
library("animation")
library("ggthemes")
library("pbapply")
library("ggpubr")
library("munsell")
library("ggmap")
library("sf")

theme_set(theme_map() + theme(legend.title = element_blank()))

# Gradient for carrying capacity colours:
grad <- c(rgb(1,1,0.95), "blue")


## Getting the google map for Denmark requires an API key:
if(FALSE){
  gmap_dk <- get_map("denmark", zoom=7)

  # Needed to avoid some weird issue in sf/ggmap:
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")),
                         c("ymin", "xmin", "ymax", "xmax"))

    # Coonvert the bbox to an sf polygon, transform it to 3857,
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

    # Overwrite the bbox of the ggmap object with the transformed coordinates
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }

  gmap_dk <- ggmap_bbox(gmap_dk)

  set_storage_folder("~/Documents/Resources/Datasets/HexScape")
  land_use_dk <- extract_corine("DK", verbose=2L)

  save(gmap_dk, land_use_dk, file="presentation_2021/gmap_dk.rda")
}
(load("presentation_2021/gmap_dk.rda"))
(load("patches/patches_denmark.rda"))


simdatafile <- "presentation_2021/dk_model_outputs/disease_progression_per_patch_dk_ddep_20_ProcessOriented.csv"
simulation <- read_csv2(simdatafile)
ccdata <- simulation %>%
  group_by(Index = patch_id, cc) %>%
  summarise(.groups="drop") %>%
  left_join(patches_dk, by="Index")



### Map 1 - just Denmark:

ggmap(gmap_dk) +
  coord_sf(crs = st_crs(3857)) +
  coord_sf(datum=3857, xlim=c(75e4, NA))
ggsave("presentation_2021/map1_denmark.jpg", width=8, height=7)


### Map 2 - with land usage:

ggmap(gmap_dk, darken=c(0.75, "white")) +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data=land_use_dk %>% st_as_sf() %>% st_transform(3857), aes(fill=CLC_LABEL1, col=CLC_LABEL1), inherit.aes=FALSE, lwd=0) +
  coord_sf(datum=3857, xlim=c(75e4, NA)) +
  theme(legend.position = "bottom")
ggsave("presentation_2021/map2_corine.jpg", width=8, height=7)


### Map 3 - converted to carrying capacity:

ggmap(gmap_dk, darken=c(0.75, "white")) +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data=patches_dk %>% st_transform(3857) %>% filter(is.na(Index)) , aes(geometry=geometry), inherit.aes=FALSE, lwd=0, fill="grey") +
  geom_sf(data=ccdata %>% st_as_sf() %>% st_transform(3857), aes(geometry=geometry, fill=cc, col=cc), inherit.aes=FALSE, lwd=0) +
  coord_sf(datum=3857, xlim=c(75e4, NA)) +
  scale_colour_gradient(low = grad[1], high = grad[2]) +
  scale_fill_gradient(low = grad[1], high = grad[2])
ggsave("presentation_2021/map3_cc.jpg", width=8, height=7)


### Map 4 - zoomed in:

# Extent of the "focussed" map in rows/columns of hexagons:
constrain <- function(x) x %>% filter(row < 50, col < 63, col < 54 | row < 35)
cczoom <- ccdata %>% st_as_sf() %>% st_transform(3857) %>% constrain()

ggmap(gmap_dk, darken=c(0.75, "white")) +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data=patches_dk %>% st_transform(3857) %>% filter(is.na(Index)) , aes(geometry=geometry), inherit.aes=FALSE, lwd=0, fill="grey") +
  geom_sf(data=cczoom, aes(geometry=geometry, fill=cc, col=cc), inherit.aes=FALSE, lwd=0) +
  coord_sf(datum=3857, xlim=c(93e4, 113e4), ylim=c(730e4, 744e4)) +
  scale_colour_gradient(low = grad[1], high = grad[2]) +
  scale_fill_gradient(low = grad[1], high = grad[2])
ggsave("presentation_2021/map4_ccz.jpg", width=8, height=5)


### For determining boundary box sizes:

ggmap(gmap_dk) +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data=patches_dk %>% st_transform(3857) %>% filter(is.na(Index)) , aes(geometry=geometry), inherit.aes=FALSE, lwd=0, fill="grey") +
  geom_sf(data=cczoom, aes(geometry=geometry, fill=col, col=col), inherit.aes=FALSE, lwd=0) +
  coord_sf(datum=3857, xlim=c(93e4, 113e4), ylim=c(732e4, 744e4)) +
  theme_light()

