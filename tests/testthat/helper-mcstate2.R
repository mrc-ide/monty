ex_simple_gamma1 <- function(shape = 1, rate = 1) {
  mcstate_model(
    parameters = "gamma",
    sample = function() rgamma(1, shape = shape, rate = rate),
    density = function(x) dgamma(x, shape = shape, rate = rate, log = TRUE),
    gradient = function(x) (shape - 1) / x - shape,
    domain = rbind(c(0, Inf)))
}


ex_simple_gaussian <- function(vcv) {
  n <- nrow(vcv)
  mcstate_model(
    parameters = letters[seq_len(n)],
    sample = make_mvnorm(vcv),
    density = make_ldmvnorm(vcv),
    gradient = make_dldnormdx(vcv),
    domain = cbind(rep(-Inf, n), rep(Inf, n)))
}
