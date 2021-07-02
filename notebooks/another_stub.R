#'
#'
#'
#'
source("../rust_asf/rscripts/eda_startup.r")
#'
#'
#'
load("patches/patches_denmark.rda")
patches_dk %>%
  filter(!is.na(Index)) %>%
  pull(area) %>%
  identity() %>%
  median()
#'
#'
#'
