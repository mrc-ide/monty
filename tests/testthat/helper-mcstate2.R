ex_simple_gamma1 <- function(shape = 1, rate = 1) {
  e <- new.env(parent = .GlobalEnv)
  e$shape <- shape
  e$rate <- rate
  with(
    e,
    mcstate_model(
      parameters = "gamma",
      direct_sample = function(rng) {
        rng$gamma(1, shape = shape, scale = 1 / rate)
      },
      density = function(x) dgamma(x, shape = shape, rate = rate, log = TRUE),
      gradient = function(x) (shape - 1) / x - rate,
      domain = rbind(c(0, Inf))))
}


ex_simple_gaussian <- function(vcv) {
  n <- nrow(vcv)
  mcstate_model(
    parameters = letters[seq_len(n)],
    direct_sample = make_rmvnorm(vcv, centred = TRUE),
    density = make_ldmvnorm(vcv),
    gradient = make_deriv_ldmvnorm(vcv),
    domain = cbind(rep(-Inf, n), rep(Inf, n)))
}
