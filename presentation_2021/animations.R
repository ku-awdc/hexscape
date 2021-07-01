## Code to make plots and animations for Mossa's presentation July 2021

library("HexScape")
library("animation")
library("ggthemes")
library("pbapply")
library("ggpubr")
library("munsell")

getmap <- function(year, iteration, simdata, patches){

  if(is.numeric(iteration)){
    simdata = simdata %>% filter(repetition_id == iteration)
  }else{
    stopifnot(iteration%in%c("max","mean"))
  }

  ## First get the numbers mid way through the specified year:
  simdata <- simdata %>%
    filter(tick <= (year*365.25)+180) %>%
    group_by(patch_id, repetition_id) %>%
    arrange(patch_id, repetition_id, -tick) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Obs = total_animals)

  if(iteration %in% c("max","mean")){
    simdata <- simdata %>%
      group_by(patch_id) %>%
      summarise(Mean = mean(total_animals),
                Max = max(total_animals), .groups="drop")

    if(iteration=="mean"){
      simdata <- simdata %>% select(Index = patch_id, Obs=Mean)
    }else if(iteration=="max"){
      simdata <- simdata %>% select(Index = patch_id, Obs=Max)
    }else{
      stop()
    }

  }else{
    simdata <- simdata %>%
      select(Index = patch_id, Obs)
  }

  return(
    simdata %>% mutate(Year = year) %>% left_join(patches, by="Index")
  )
}


(load("patches/patches_denmark.rda"))

simulation <- read_csv2("presentation_2021/dk_model_outputs/disease_progression_per_patch_dk_ddep_20_ProcessOriented.csv")

# Extent of the "focussed" map in rows/columns of hexagons:
range <- c(50,50)

# Number of years in the simulation:
maxyears <- floor(max(simulation$tick)/365.25)

ccdata <- simulation %>%
  group_by(Index = patch_id, cc) %>%
  summarise(.groups="drop") %>%
  left_join(patches_dk, by="Index")

theme_set(theme_map() + theme(legend.title = element_blank()))
#theme_set(theme_map())

if(FALSE){
library(tmap)
tmap_mode("plot")
tmap_options(check.and.fix = TRUE)
tm_shape(patches_dk) +
  tm_polygons("LU_High")
}

# All of Denmark
pt <- ggplot(ccdata) +
  aes(fill=cc, col=cc, geometry=geometry) +
  geom_sf(lwd=0) +
  scale_x_continuous(labels=NULL)
print(pt)
ggsave("denmark_full.pdf")

# Just the focussed area:
pt <- ggplot(ccdata %>% filter(row < range[1], col < range[2])) +
  aes(fill=cc, col=cc, geometry=geometry) +
  geom_sf(lwd=0)
print(pt)
ggsave("denmark_sw.pdf")


### Mean and max total number of animals after 1:maxyears years:
plotdata <- 1:maxyears %>%
  as.list() %>%
  pblapply(getmap, iteration="mean", simdata=simulation, patches=patches_dk)

maxobs <- sapply(plotdata, function(x) max(x$Obs)) %>% max()

cpt <- ggplot(ccdata %>% filter(row < range[1], col < range[2])) +
  aes(fill=cc, col=cc, geometry=geometry) +
  geom_sf(lwd=0) +
  ggtitle("Carrying Capacity") +
  scale_fill_gradient(limits = c(0, maxobs)) +
  scale_colour_gradient(limits = c(0, maxobs)) +
  theme(legend.pos="none")
cpt

#pdf("temporal.pdf", width=8, height=4)
unlink("presentation_2021/figs", recursive=TRUE)
dir.create("presentation_2021/figs")
pblapply(plotdata, function(x){
  allyears <- gsub(" ", "0", format(1:maxyears))
  pdat <- x %>%
    filter(row < range[1], col < range[2])
  pt <- ggplot(pdat) +
    aes(fill=Obs, col=Obs, geometry=geometry) +
    geom_sf(lwd=0) +
    ggtitle(str_c("Animals after ", x$Year[1], " years")) +
    scale_fill_gradient(limits = c(0, maxobs)) +
    scale_colour_gradient(limits = c(0, maxobs))
  ggpubr::ggarrange(pt, cpt, ncol=2)
  ggsave(str_c("presentation_2021/figs/plot", allyears[x$Year[1]], ".jpg"), width=8, height=4)
  invisible(TRUE)
})
#dev.off()

unlink("presentation_2021/plot.mp4")
system('ffmpeg -framerate 1 -pattern_type  glob -i "presentation_2021/figs/plot*.jpg" "presentation_2021/plot.mp4"')
unlink("presentation_2021/figs", recursive=TRUE)

