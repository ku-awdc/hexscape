

load("patches/patches_denmark.rda")
#'
#'
#'
#' THere are patches that are less area than the ideal.
maximal_cc <- 35 / patches_dk %>% drop_na(Index) %>% pull("area") %>% max()
patches_dk %>%
  mutate(cc_relative = LU_High * maximal_cc + LU_Medium * maximal_cc / 2 + LU_Low * maximal_cc / 4,
         cc = cc_relative * area ) %>%

  identity() -> patches_cc_dk
#'
#'
#'
#'
patches_cc_dk %>% {
    ggplot(.) +
      aes(cc) +
      geom_freqpoly()
}
#'
#'
patches_cc_dk %>% {
  ggplot(.) +
    aes(fill = cc_relative) +
    geom_sf() +
    scale_fill_viridis_c()
}

