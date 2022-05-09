library(tidyverse)

rho <- 1.5
mean <- 50

# Calculate parameters for Weibull:
intercept <- (log(mean) - log(gamma(1 + 1 / rho))) * -rho
scale <- exp(intercept)^(-1 / rho)

sample_settling_times <- function(n, n_steps, scale, shape, beta_steps) {
    u <- runif(n)
    (
        -log(u) / (scale * exp(beta_steps * n_steps))
    )**(1 / shape)
}

sample_settling_times(100, n_steps = 3, scale = 0.9, shape = 1, beta_steps = 0)
expand_grid(
    n = 100, n_steps = seq.default(0, 25, by = 4), scale = 0.1, shape = 1, beta_steps = 0.1
) %>%
    rowid_to_column() %>%
    rowwise() %>%
    mutate(
        settling_times =
            sample_settling_times(n, n_steps, scale, shape, beta_steps) %>%
                list()
    ) %>%
    ungroup() %>%
    unnest(settling_times)  %>%
    ggplot(aes(settling_times, group = rowid)) +
    geom_freqpoly() +
    facet_wrap(~n_steps)
