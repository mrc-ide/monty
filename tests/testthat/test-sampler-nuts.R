test_that("can run nuts", {
  m <- monty_example("banana")
  sampler <- monty_sampler_nuts(epsilon = 0.1, max_treedepth = 1000)

  set.seed(1)
  res <- monty_sample(m, sampler, 30)

  set.seed(1)
  reference <- reference_nuts()(epsilon = 0.1, max_treedepth = 1000)
  expected <- monty_sample(m, reference, 30)

  expect_equal(res, expected)
})


test_that("can continue nuts without state", {
  m <- monty_example("banana")
  sampler <- monty_sampler_nuts(epsilon = 0.1, max_treedepth = 1000)

  set.seed(1)
  res1 <- monty_sample(m, sampler, 30, restartable = TRUE)

  set.seed(1)
  res2a <- monty_sample(m, sampler, 10, restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 20, restartable = TRUE)

  expect_equal(res2b, res1)
})


test_that("nuts requires gradients", {
  m <- monty_model(list(density = function(x) dnorm(x, log = TRUE),
                        parameters = "a"))
  sampler <- monty_sampler_nuts(epsilon = 0.1)

  expect_error(
    monty_sample(m, sampler, 30),
    "No-U-Turn Sampler requires a gradient but 'model' does not provide one",
    fixed = TRUE)
})


test_that("nuts requires deterministic models", {
  set.seed(1)
  m <- ex_sir_filter_posterior()
  sampler <- monty_sampler_nuts(epsilon = 0.1)

  expect_error(
    monty_sample(m, sampler, 30),
    "No-U-Turn Sampler requires deterministic models, but 'model' is stochastic",
    fixed = TRUE)
})


test_that("nuts does not support simultaneous runner yet", {
  m <- monty_example("banana")
  sampler <- monty_sampler_nuts(epsilon = 0.1)
  runner <- monty_runner_simultaneous()

  expect_error(
    monty_sample(m, sampler, 30, n_chains = 3, runner = runner),
    "Can't use the simultaneous runner with this sampler",
    fixed = TRUE)
})