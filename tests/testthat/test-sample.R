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
  model1 <- ex_simple_gamma1()
  model2 <- ex_simple_gamma1()
  ## A bit silly; more likely this will be a bug on our end writing
  ## the sampler, but this is possible when running particle filter
  ## models.
  model2$density <- function(...) {
    runif(1)
    model1$density(...)
  }
  sampler <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_no_warning(
    mcstate_sample(model1, sampler, 100))
  expect_warning(
    mcstate_sample(model2, sampler, 100),
    "Detected use of R's random number generators")
})
