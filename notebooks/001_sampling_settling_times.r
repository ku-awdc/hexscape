library(tidyverse)

#' h(t|x(t)) = h_0(t) exp(beta_t z(t) + beta_transpose x)
sample_settling_times <- function(n, n_steps, scale, shape, beta_t) {
    stopifnot(
        n >= 0,
        scale > 0,
        shape > 0,
        n_steps > 0,
        "if `beta_t` is 0, then we'd get infinities" = beta_t > 0
    )
    u <- runif(n)
    (
        (1 / n_steps) * log(
            1 + ((1 + shape) * (-log(u))) /
                # (beta_t * exp(tcrossprod(beta, x) * scale * shape))
                (beta_t * exp(1 * scale * shape))
        )
    )**(1 / (1 + shape))
}

sample_settling_times(100, n_steps = 1, scale = 1, shape = 1, beta_t = 10)

expand_grid(
    n = 100,
    n_steps = seq.default(1, 25, by = 4), scale = 55, shape = 1.2, beta_t = 0.1
) %>%
    rowid_to_column() %>%
    rowwise() %>%
    mutate(
        settling_times =
            sample_settling_times(n, n_steps, scale, shape, beta_t) %>%
                list()
    ) %>%
    ungroup() %>%
    unnest(settling_times) %>%
    # print()
    # filter(settling_times %>% is.na())
    ggplot(aes(round(settling_times), group = rowid)) +
    geom_bar() +
    facet_wrap(~n_steps)
