## Parameters for the time to event:
rho <- 1.5
mean <- 50

# Calculate parameters for Weibull:
intercept <- (log(mean) - log(gamma(1 + 1 / rho))) * -rho
scale <- exp(intercept)^(-1 / rho)

# Just needed for plotting:
MaxT <- 200

# This is the density of distances travelled:
curve(dweibull(x, rho, scale), from = 0, to = MaxT, ylab = "", xlab = "distance")

# And this is the cumulative proportion by distances travelled:
curve(pweibull(x, rho, scale), from = 0, to = MaxT, ylab = "", xlab = "distance")

# So the cumulative hazard of settling by x km is:
x <- 10
(p_x <- pweibull(x, rho, scale))

# And the integrated (total) hazard of settling between x km and y km is:
y <- 15
p_y <- pweibull(y, rho, scale)
(p_y - p_x)

# So the total hazard of settling between x km and y km conditional on having made it as far as x km is:
(p_y - p_x) / (1 - p_x)

# And the instantaneous hazard of settling at x km conditional on having made it as far as x km is:
dweibull(x, rho, scale) / (1 - pweibull(x, rho, scale))

# With small enough x/rho then this is the same as:
rho * x^(rho - 1) * exp(intercept)
# BUT as x gets larger, they diverge...
# intercept <- (log(mean) - log(gamma(1 + 1 / rho))) * -rho # nolint
# intercept <- log(1 / rho) # see [source](https://stats.stackexchange.com/questions/53273/weibull-regression-with-known-intercept-in-r)
# intercept <- rho**(-scale)
curve(abs(
    (dweibull(x, rho, scale) / (1 - pweibull(x, rho, scale))) -
        rho * x^(rho - 1) * exp(intercept)
), xlim = c(0, 1000))