## Code to make plots and animations for Mossa's presentation July 2021

library("HexScape")
library("animation")
library("ggthemes")
library("pbapply")
library("ggpubr")
library("sf")
library(glue)

### Change to adjust the name of the simulated data file:
# scenario_name <- c(
#   "dk_ddep_720",
#   "dk_no_ddep_720",
#   "dk_ddep_20",
#   "dk_no_ddep_20"
# )
# "../rust_asf/outputs/"
# scenario_name <- scenario_name[4]
# scenario_name <- "dk_extreme_turnout"
# scenario_name <- "original_dk_ddep_1720"
# scenario_name <- scenarios[2]
cat(glue("Scenario name: {scenario_name}"), scenario_name)
# simdatafile <- glue("presentation_2021/dk_model_outputs/disease_progression_per_patch_{scenario_name}_ProcessOriented.csv")
simdatafile <- glue("../rust_asf/outputs/disease_progression_per_patch_{scenario_name}_ProcessOriented.csv")
outfolder <- "presentation_2021/figs"

## Change to zero to remove ASF dots:
asf_size <- 0.75
asf_alpha <- 0.5

theme_set(theme_map() + theme(legend.title = element_blank()))
# Gradient for carrying capacity colours:
grad <- c(rgb(1,1,0.95), "blue")

getmap <- function(year, iteration, simdata, patches){

  if(is.numeric(iteration)){
    simdata = simdata %>%
      filter(repetition_id == iteration)
  }else{
    stopifnot(iteration %in% c("max","mean"))
  }

  ## First get the numbers mid way through the specified year:
  simdata <- simdata %>%
    filter(tick <= (year*365.25)+180) %>%
    group_by(patch_id, repetition_id) %>%
    arrange(patch_id, repetition_id, tick) %>%
    mutate(CumInf = cumsum(infected) > 0) %>%
    slice(n()) %>%
    ungroup() %>%
    mutate(Obs = total_animals)

  if(iteration %in% c("max","mean")){
    simdata <- simdata %>%
      group_by(patch_id) %>%
      summarise(Mean = mean(total_animals),
                Max = max(total_animals),
                CumInf = mean(CumInf),
                .groups="drop")

    if(iteration=="mean"){
      simdata <- simdata %>% select(Index = patch_id, Obs=Mean, CumInf)
    }else if(iteration=="max"){
      simdata <- simdata %>% select(Index = patch_id, Obs=Max, CumInf)
    }else{
      stop()
    }

  }else{
    simdata <- simdata %>%
      select(Index = patch_id, Obs, CumInf)
  }

  return(
    simdata %>% mutate(Year = year) %>% left_join(patches, by="Index")
  )
}

(load("patches/patches_denmark.rda"))

simulation <- read_csv2(simdatafile)

# Extent of the "focussed" map in rows/columns of hexagons:
constrain <- function(x) x %>%
  filter(row < 50, col < 63, col < 54 | row < 35)

# Number of years in the simulation:
maxyears <- floor(max(simulation$tick)/365.25)
cat(glue("{maxyears}"))
ccdata <- simulation %>%
  # just use the first repetition's CCs from the first iteration
  filter(repetition_id == 1, tick == 2) %>%
  group_by(Index = patch_id, cc) %>%
  summarise(.groups="drop") %>%
  left_join(patches_dk, by="Index") %>%
  force()

#' Limit the number of used iteration until the entirety of the pipeline Works.
#'
simulation <- simulation %>%
  filter((tick > 2 & between(repetition_id, 1, 10)) |  (tick == 2 & repetition_id == 1))


### Mean and max total number of animals after 1:maxyears years:
plotdata <- 1:maxyears %>%
  as.list() %>%
  pblapply(getmap, iteration="mean", simdata=simulation, patches=patches_dk)

maxobs <- sapply(plotdata, function(x) max(x$Obs)) %>% max()

cpt <- {
  ccdata_constrain <-ccdata %>% constrain()
  ggplot() +
    geom_sf(data=ccdata_constrain,
            aes(fill=cc, col=cc, geometry=geometry), lwd=0) +
    ggtitle("Carrying Capacity") +
    theme(legend.pos="none") +
    scale_colour_gradient(limits = ccdata_constrain$cc %>% range(),
                          low = grad[1], high = grad[2]) +
    scale_fill_gradient(limits = ccdata_constrain$cc %>% range(),
                        low = grad[1], high = grad[2]) +
    NULL}
# cpt

### Create daily jpg:
# unlink(outfolder, recursive=TRUE)
dir.create(outfolder)
pblapply(plotdata, function(x){
  allyears <- gsub(" ", "0", format(1:maxyears))
  asfpts <- x %>% constrain() %>% filter(CumInf > 0)
  pt <- ggplot(x %>% constrain()) +
    aes(fill=Obs, col=Obs, geometry=geometry) +
    geom_sf(lwd=0) +
    ## Comment out this line to remove ASF dots:
    geom_sf(data=asfpts, aes(geometry=centroid), col="red", fill="red", pch=20, size=asf_size, alpha=asf_alpha) +
    ggtitle(str_c("Animals after ", x$Year[1], ifelse(x$Year[1]==1, " year", " years"))) +
    scale_colour_gradient(limits = c(0, maxobs), low = grad[1], high = grad[2]) +
    scale_fill_gradient(limits = c(0, maxobs), low = grad[1], high = grad[2])
  # pt
  # ggpubr::ggarrange(pt, cpt, ncol=2)
  ggsave(
    plot = ggpubr::ggarrange(pt, cpt, ncol = 2),
    str_c(outfolder, "/plot_", scenario_name, "_", allyears[x$Year[1]], ".jpg"), width=8, height=4)
  invisible(TRUE)
})

### To create the mp4 requires ffmpeg (not needed now):
# unlink(glue("presentation_2021/plot_{scenario_name}.mp4"))
system(glue('ffmpeg -y -framerate 1 -i "presentation_2021/figs/plot_{scenario_name}_%2d.jpg" "presentation_2021/plot_{scenario_name}.mp4"'))

