ex_simple_gamma1 <- function(shape = 1, rate = 1) {
  mcstate_model(
    parameters = "gamma",
    direct_sample = function(rng) rng$gamma(1, shape = shape, scale = 1 / rate),
    density = function(x) dgamma(x, shape = shape, rate = rate, log = TRUE),
    gradient = function(x) (shape - 1) / x - rate,
    domain = rbind(c(0, Inf)))
}
