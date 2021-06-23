

#' As it stands right now this is the carrying/breeding capacity for Southern
#' Jutland alone, and not the entirety of Denmark.
#'
#' This is saved as well, so, next time this is changed to the entire DK, it
#' will overwrite.
#'
#' FIXME: Pay attention to that.
#'
#'
source("../asf_phd/R/eda_startup.r")
library(sf)

#' this RDA comes from `denmark_estonia.R` file.
#'
#'
load("south_jutland_patches.rda")
#'
#'
#'

# These were not used here.
distances
map_jutland
neighbours

# see last section
# patches %>%
#   as_tibble() %>%
#   filter(!is.na(Index)) %>%
#   select(Index, row, col, area) %>%
#
#   write_csv("../paper_eco_disease_model/data/south_jutland_patches.csv")

#'
#' We have to determine a maximal breeding capacity that works...
maximal_carrying_capacity <- 25
#' sow -> {avg 6 piglets, males}
FemPropGroup     <- mean(c(14.7, 18.1, 12.8, 14.6))
MalPropGroup     <- mean(c(9.2, 9, 5.7, 7.9))
SubAPropGroup    <- mean(c(33.4, 34.3, 22.8, 24.1))
PigletPropGroup  <- mean(c(42.7, 38.6, 58.7, 53.4))
FemPropGroup
MalPropGroup
SubAPropGroup
PigletPropGroup
sum(FemPropGroup,
    MalPropGroup,
    SubAPropGroup,
    PigletPropGroup)

cc_relative_weights <- c(Passable = 0,
                         Low = 0,
                         Medium = 15,
                         High = maximal_carrying_capacity)
cc_relative_weights

maximal_area <- patches %>% filter(!is.na(Index)) %>% pull(area) %>% max

patches %>%
  as_tibble() %>%
  filter(!is.na(Index)) %>%
  select(starts_with("LU"), area, geometry, Index) %>%
  mutate(prop_area = area / maximal_area) %>%

  pack(LU = starts_with("LU_", ignore.case = FALSE)) %>%
  rowwise() %>%
  mutate(carrying_capacity = across(LU, ~ sum(.x * cc_relative_weights)),
         #TODO: add added effect of having arable land next to a forest.
         # Parameter: ([forest] x [arable land])
         carrying_capacity = prop_area * carrying_capacity) %>%
  unpack(LU) %>%
  unpack(carrying_capacity, names_sep = "_") %>%
  rename(carrying_capacity = carrying_capacity_LU) %>%



  identity() ->
  carrying_df

carrying_df %>% {
  ggplot(., aes(carrying_capacity)) +

    geom_histogram()
}


plot.new()
curve(dgamma(x, 1.247445, scale = 0.2253802), 0, 25)
curve(dexp(x, rate = 0.31), 0, 25, add = TRUE)

glm(I(carrying_capacity+1e3*.Machine$double.eps) ~ 1,
    data = carrying_df %>%
      # filter(carrying_capacity > 1),
      # filter(carrying_capacity > 0),
      identity(),
    family = Gamma)

# Cool approach tog
# optimize(function(x)
#   sum(dexp(carrying_df$carrying_capacity, x, log = TRUE)), c(0, 1e6),
#   maximum = TRUE)

#'
#' 1 3 6

carrying_df %>%
  filter(carrying_capacity>0) %>%
  # mutate(breeding_capacity = carrying_capacity *carrying_dfV)
  # summarise(min(carrying_capacity),
  #           pracma::Mode(carrying_capacity),
  #           max(carrying_capacity))
  summarise(quantile(carrying_capacity, seq(0.1, .9, length.out = 5)) %>% list()) %>%
  unnest()

carrying_df %>% {
  # filter(carrying_capacity>0) %>% {
  ggplot(.) + geom_sf(aes(geometry = geometry, fill = carrying_capacity/area * 1e6))
}


#' Blocked cells are not included in the spatial configuration.
#' Passable cells are cells with carrying_capacity zero?
#'
carrying_df %>%

  mutate(habitat_label = cut.default(carrying_capacity, breaks = c(-Inf, 1, Inf))) %>%
  print() %>%
  pull(habitat_label) %>%
  table %>%
  print() %>%
  prop.table()
#'
#'
carrying_df %>%
  mutate(breeding_capacity = carrying_capacity * FemPropGroup / 100,
         breeding_capacity = round(breeding_capacity)) %>%
  select(Index, carrying_capacity, breeding_capacity) ->
  carrying_df
#'
#'
#' Add labels
#'
carrying_df %>%
  mutate(patch_type = case_when(breeding_capacity == 0 ~ "Passable",
                                TRUE ~ "Habitable")) ->
  carrying_df
#'
#'
carrying_df$patch_type %>%
  table() %>% print() %>% prop.table() %>% {
    # .[] <- lapply(., pretty)
    .
  }
#' ###  Output the Carrying / Breeding Capacity
#'
#'
#'
#' Output the data-frame for this in some consumable format.
carrying_df %>%
  write_csv("../paper_eco_disease_model/data/south_jutland_carrying_capacity.csv") %>%
  write_rds("../paper_eco_disease_model/data/breeding_capacity_based_on_landcover.rds")
#'
#'
#'
#'

patches %>%
  as_tibble() %>%
  filter(!is.na(Index)) %>%

  inner_join(carrying_df, by = "Index") %>%
  select(Index, row, col, area, names(carrying_df)) %>%

  write_csv("../paper_eco_disease_model/data/south_jutland_patches_and_carrying_cap.csv")

#'
#'
#' ### Discarded plots

patches %>% {
  ggplot(.) + geom_sf() +
    geom_sf(aes(fill = starting_point), data = . %>% slice(11) %>% mutate(starting_point = TRUE))
}
