## Code to make plots and animations for Mossa's presentation July 2021

library("HexScape")
library("animation")
library("ggthemes")


(load("patches/patches_denmark.rda"))

simulation <- read_csv2("presentation_2021/disease_progression_per_patch_dk_world_dev_ProcessOriented.csv")

plotdata <- patches_dk %>%
  left_join(simulation %>% filter(repetition_id==1, tick==2) %>% mutate(Index = patch_id), by="Index") %>%
  mutate(MaxDensity = cc / area * 1e6)

theme_set(theme_map() + theme(legend.title = element_blank()))
#theme_set(theme_map())

ggplot(plotdata) +
  aes(fill=cc, col=cc) +
  geom_sf(lwd=0) +
  scale_x_continuous(labels=NULL)
ggsave("denmark_full.pdf")

ggplot(plotdata %>% filter(row < 105, col < 70)) +
  aes(fill=cc, col=cc) +
  geom_sf(lwd=0)
ggsave("denmark_sw.pdf")


### Average total number of animals after 1000 ticks:
simulation %>%
  filter(tick<=7000) %>%
  group_by(patch_id, repetition_id) %>%
  arrange(patch_id, repetition_id, -tick) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(patch_id) %>%
  summarise(Average = mean(total_animals), .groups="drop") ->
average

simplotdata <- patches_dk %>%
  left_join(average %>% mutate(Index = patch_id), by="Index")

ggplot(simplotdata %>% filter(row < 105, col < 70)) +
  aes(fill=Average, col=Average) +
  geom_sf(lwd=0)
