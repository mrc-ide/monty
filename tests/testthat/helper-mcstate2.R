ex_simple_gamma1 <- function(shape = 1, rate = 1) {
  e <- new.env(parent = .GlobalEnv)
  e$shape <- shape
  e$rate <- rate
  with(
    e,
    mcstate_model(list(
      parameters = "gamma",
      direct_sample = function(rng) {
        rng$gamma(1, shape = shape, scale = 1 / rate)
      },
      density = function(x) dgamma(x, shape = shape, rate = rate, log = TRUE),
      gradient = function(x) (shape - 1) / x - rate,
      domain = rbind(c(0, Inf)))))
}


ex_simple_gaussian <- function(vcv) {
  n <- nrow(vcv)
  mcstate_model(list(
    parameters = letters[seq_len(n)],
    direct_sample = make_rmvnorm(vcv, centred = TRUE),
    density = make_ldmvnorm(vcv),
    gradient = make_deriv_ldmvnorm(vcv),
    domain = cbind(rep(-Inf, n), rep(Inf, n))))
}


ex_banana <- function(sd = 0.5) {
  mcstate_model(list(
    parameters = c("theta1","theta2"),
    direct_sample = function(rng) {
      theta <- rng$random_normal(1)
      cbind(rng$normal(1, theta^2, sd), theta)
    },
    density = function(x) {
      dnorm(x[2], log = T) + dnorm((x[1]-x[2]^2)/sd, log = T)
    },
    gradient = function(x){
      c((x[2]^2-x[1])/sd^2,
        -x[2]+2*x[2]*(x[1]-x[2]^2)/sd^2)
    },
    domain = cbind(rep(-Inf, 2), rep(Inf, 2))))
}
