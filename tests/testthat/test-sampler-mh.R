test_that("can draw samples from a trivial model", {
  m <- local({
    shape <- 1
    rate <- 1
    mcstate_model(
      parameters = "gamma",
      sample = function() rgamma(1, shape = shape, rate = rate),
      density = function(x) dgamma(x, shape = shape, rate = rate, log = TRUE),
      gradient = function(x) (shape - 1) / x - shape,
      domain = rbind(c(0, Inf)))
  })
  sampler <- mcstate_sampler_metropolis_hastings(vcv = matrix(0.01, 1, 1))
  res <- mcstate_sample(m, sampler, 100)
  expect_equal(names(res), c("pars", "density"))
})


test_that("validate construction of mh sampler", {
  expect_error(
    mcstate_sampler_metropolis_hastings(),
    "One of 'proposal' or 'vcv' must be given")
  expect_error(
    mcstate_sampler_metropolis_hastings(identity, matrix(1, 1, 1)),
    "Only one of 'proposal' or 'vcv' may be given")
})


test_that("validate sampler against model on initialisation", {
  m <- local({
    shape <- 1
    rate <- 1
    mcstate_model(
      parameters = "gamma",
      sample = function() rgamma(1, shape = shape, rate = rate),
      density = function(x) dgamma(x, shape = shape, rate = rate, log = TRUE),
      gradient = function(x) (shape - 1) / x - shape,
      domain = rbind(c(0, Inf)))
  })

  state <- list(pars = m$sample(), density = -Inf)
  sampler1 <- mcstate_sampler_metropolis_hastings(vcv = diag(1) * 0.01)
  sampler2 <- mcstate_sampler_metropolis_hastings(vcv = diag(2) * 0.01)

  expect_no_error(sampler1$initialise(state, model))
  expect_error(
    sampler2$initialise(state, model),
    "Incompatible length parameters (1) and vcv (2)",
    fixed = TRUE)
})
