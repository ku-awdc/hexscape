## Code to make plots and animations for Mossa's presentation July 2021

library("HexScape")
library("animation")
library("ggthemes")
library("pbapply")
library("ggpubr")
library("sf")

theme_set(theme_map() + theme(legend.title = element_blank()))
# Gradient for carrying capacity colours:
grad <- c(rgb(1,1,0.95), "blue")


(load("patches/patches_denmark.rda"))

simulation <- read_csv2("presentation_2021/dk_model_outputs/disease_progression_per_patch_dk_ddep_20_ProcessOriented.csv")

# Extent of the "focussed" map in rows/columns of hexagons:
constrain <- function(x) x %>% filter(row < 50, col < 63, col < 54 | row < 35)

# Number of years in the simulation:
maxyears <- floor(max(simulation$tick)/365.25)

ccdata <- simulation %>%
  group_by(Index = patch_id, cc) %>%
  summarise(.groups="drop") %>%
  left_join(patches_dk, by="Index")


### Mean and max total number of animals after 1:maxyears years:
plotdata <- 1:maxyears %>%
  as.list() %>%
  pblapply(getmap, iteration="mean", simdata=simulation, patches=patches_dk)

maxobs <- sapply(plotdata, function(x) max(x$Obs)) %>% max()

cpt <- ggplot() +
  geom_sf(data=ccdata %>% constrain(), aes(fill=cc, col=cc, geometry=geometry), lwd=0) +
  ggtitle("Carrying Capacity") +
  theme(legend.pos="none") +
  scale_colour_gradient(limits = c(0, maxobs), low = grad[1], high = grad[2]) +
  scale_fill_gradient(limits = c(0, maxobs), low = grad[1], high = grad[2])
cpt

### Create daily jpg:
unlink("presentation_2021/figs", recursive=TRUE)
dir.create("presentation_2021/figs")
pblapply(plotdata, function(x){
  allyears <- gsub(" ", "0", format(1:maxyears))
  pt <- ggplot(x %>% constrain()) +
    aes(fill=Obs, col=Obs, geometry=geometry) +
    geom_sf(lwd=0) +
    ggtitle(str_c("Animals after ", x$Year[1], ifelse(x$Year[1]==1, " year", " years"))) +
    scale_colour_gradient(limits = c(0, maxobs), low = grad[1], high = grad[2]) +
    scale_fill_gradient(limits = c(0, maxobs), low = grad[1], high = grad[2])
  ggpubr::ggarrange(pt, cpt, ncol=2)
  ggsave(str_c("presentation_2021/figs/plot", allyears[x$Year[1]], ".jpg"), width=8, height=4)
  invisible(TRUE)
})

### To create the mp4 requires ffmpeg (not needed now):
# unlink("presentation_2021/plot.mp4")
# system('ffmpeg -framerate 1 -pattern_type  glob -i "presentation_2021/figs/plot*.jpg" "presentation_2021/plot.mp4"')

