## Code to generate a map

library("tidyverse")
library("sf")

(load("patches/patches_denmark.rda"))

# Change LU_High to the number of wild boar:
ggplot(patches_dk %>% filter(row < 70, col < 70)) +
  aes(fill=LU_High, col=LU_High) +
  geom_sf() +
  ggtitle("Time Step: 1")

# And do a small animation over time steps within a single iteration

# Then do the average number per patch after e.g. 10 years and plot that

