test_that("can validate sample inputs", {
  model <- ex_simple_gamma1()
  sampler <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_error(mcstate_sample(NULL, NULL, 100),
               "Expected 'model' to be an 'mcstate_model'")
  expect_error(mcstate_sample(model, NULL, 100),
               "Expected 'sampler' to be an 'mcstate_sampler'")
  expect_error(mcstate_sample(model, sampler, 100, c(1, 2)),
               "Unexpected initial parameter length 2; expected 1")
})


test_that("sampler return value contains history", {
  model <- ex_simple_gamma1()
  sampler <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)
  res <- mcstate_sample(model, sampler, 100, 1)
  expect_setequal(names(res), c("pars", "density", "details"))
  expect_true(is.matrix(res$pars))
  expect_equal(dim(res$pars), c(101, 1))
  expect_equal(dimnames(res$pars), list(NULL, "gamma"))
  expect_length(res$density, 101)
  expect_equal(res$density, apply(res$pars, 1, model$density))
  expect_equal(unname(res$pars[1, ]), 1)
  expect_null(res$details)
})


test_that("warn if model uses R's rng", {
  ## This example is a little contrived because actually sampling
  ## initial conditions is ok; we'll guarantee that happens in series
  ## anyway.  Bigger concerns would be if the sampler uses random
  ## numbers or if in a stochastic model we use random numbers while
  ## computing the density.
  model <- ex_simple_gamma1()
  model$direct_sample <- function(rng) rgamma(1, 1, 1)
  sampler <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_warning(
    mcstate_sample(model, sampler, 100),
    "Detected use of R's random number generators")
  expect_no_warning(
    mcstate_sample(model, sampler, 100, 1))
})
