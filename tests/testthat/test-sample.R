test_that("can validate sample inputs", {
  model <- ex_simple_gamma1()
  sampler <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_error(mcstate_sample(NULL, NULL, 100),
               "Expected 'model' to be an 'mcstate_model'")
  expect_error(mcstate_sample(model, NULL, 100),
               "Expected 'sampler' to be an 'mcstate_sampler'")
  expect_error(mcstate_sample(model, sampler, 100, runner = TRUE),
               "Expected 'runner' to be an 'mcstate_runner'")
  expect_error(mcstate_sample(model, sampler, 100, c(1, 2)),
               "Unexpected length for vector 'initial' (given 2, expected 1)",
               fixed = TRUE)
})


test_that("sampler return value contains history", {
  model <- ex_simple_gamma1()
  sampler <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)
  res <- mcstate_sample(model, sampler, 100, 1)
  expect_setequal(names(res), c("pars", "density", "details", "chain"))
  expect_true(is.matrix(res$pars))
  expect_equal(dim(res$pars), c(101, 1))
  expect_equal(dimnames(res$pars), list(NULL, "gamma"))
  expect_length(res$density, 101)
  expect_equal(res$density, apply(res$pars, 1, model$model$density))
  expect_equal(unname(res$pars[1, ]), 1)
  expect_null(res$details)
})


test_that("warn if model uses R's rng", {
  model1 <- ex_simple_gamma1()
  model2 <- ex_simple_gamma1()
  ## A bit silly; more likely this will be a bug on our end writing
  ## the sampler, but this is possible when running particle filter
  ## models.
  model2$model$density <- function(...) {
    runif(1)
    model1$model$density(...)
  }
  sampler <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_no_warning(
    mcstate_sample(model1, sampler, 100))
  expect_warning(
    mcstate_sample(model2, sampler, 100),
    "Detected use of R's random number generators")
})


test_that("sample initial state if not provided, with a single chain", {
  m <- ex_simple_gamma1()
  g1 <- mcstate_rng$new(seed = 42)
  g2 <- mcstate_rng$new(seed = 42)

  res <- initial_parameters(NULL, m, list(g1))
  expect_equal(res, matrix(m$direct_sample(g2), 1, 1))
})


test_that("sample initial state if not provided, with multiple chains", {
  m <- ex_simple_gamma1()
  g1 <- initial_rng(3, 42)
  g21 <- mcstate_rng$new(seed = 42)
  g22 <- mcstate_rng$new(seed = 42)$long_jump()
  g23 <- mcstate_rng$new(seed = 42)$long_jump()$long_jump()

  res <- initial_parameters(NULL, m, g1)
  expect_equal(dim(res), c(3, 1))
  expect_equal(res[1, 1], m$direct_sample(g21))
  expect_equal(res[2, 1], m$direct_sample(g22))
  expect_equal(res[3, 1], m$direct_sample(g23))
})


test_that("validate that initial have correct size for list inputs", {
  m <- ex_simple_gamma1()
  r <- initial_rng(2)
  expect_equal(
    initial_parameters(list(1, 2), m, r),
    cbind(c(1, 2)))
  expect_error(
    initial_parameters(list(), m, r),
    "Unexpected length for list 'initial' (given 0, expected 2)",
    fixed = TRUE)
  expect_error(
    initial_parameters(list(c(1, 2), c(1, 2, 3)), m, r),
    "Unexpected initial parameter length; expected 1",
    fixed = TRUE)
})


test_that("validate that initial have correct size for matrix inputs", {
  m <- ex_simple_gamma1()
  r <- initial_rng(2)
  expect_equal(
    initial_parameters(matrix(c(1, 2), 2, 1), m, r),
    cbind(c(1, 2)))
  expect_error(
    initial_parameters(matrix(1, 2, 2), m, r),
    "Unexpected number of columns in 'initial' (given 2, expected 1)",
    fixed = TRUE)
  expect_error(
    initial_parameters(matrix(1, 3, 1), m, r),
    "Unexpected number of rows in 'initial' (given 3, expected 2)",
    fixed = TRUE)
})


test_that("validate that initial have correct size for vector inputs", {
  m <- ex_simple_gamma1()
  r <- initial_rng(2)
  expect_equal(
    initial_parameters(1, m, r),
    cbind(c(1, 1)))
  expect_error(
    initial_parameters(c(1, 2), m, r),
    "Unexpected length for vector 'initial' (given 2, expected 1)",
    fixed = TRUE)
})


test_that("can run more than one chain, in parallel", {
  model <- ex_simple_gamma1()
  sampler <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- mcstate_sample(model, sampler, 100, 1, n_chains = 2)

  set.seed(1)
  res2 <- mcstate_sample(model, sampler, 100, 1, n_chains = 2,
                         runner = mcstate_runner_parallel(2))

  expect_identical(res1, res2)
})


test_that("need a direct sample function in order to start sampling", {
  model1 <- ex_simple_gamma1()
  model2 <- mcstate_model(list(density = model1$model$density),
                          parameters = model1$parameters,
                          domain = model1$domain)
  sampler <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_no_error(
    mcstate_sample(model1, sampler, 10))
  expect_error(
    mcstate_sample(model2, sampler, 10),
    "'initial' must be provided with this model")
})
