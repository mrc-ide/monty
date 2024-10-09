test_that("require multiple parameter support for simultaneous runner", {
  m <- monty_model(list(density = function(x) dnorm(x, log = TRUE),
                          parameters = "a"))
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  runner <- monty_runner_simultaneous()
  expect_error(
    monty_sample(m, sampler, 100, initial = 1, runner = runner),
    "monty_runner_simultaneous requires support for multiple parameters")
})


test_that("require multiple parameter support for simultaneous runner", {
  m <- monty_model(list(density = function(x) dnorm(x, log = TRUE),
                          set_rng_state = identity,
                          get_rng_state = identity,
                          parameters = "a"),
                     monty_model_properties(is_stochastic = TRUE,
                                              allow_multiple_parameters = TRUE))
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  runner <- monty_runner_simultaneous()
  expect_error(
    monty_sample(m, sampler, 100, initial = 1, runner = runner),
    "Can't yet use multiple parameter sets with stochastic model",
    fixed = TRUE)
})


test_that("can use different covariance kernels on different chains", {
  m <- ex_simple_gamma1()
  vcv <- array(1 * 10^c(-4, -3, -2, -1, 0), c(1, 1, 5))
  sampler <- monty_sampler_random_walk(vcv = vcv)
  runner <- monty_runner_simultaneous()
  n_steps <- 30

  set.seed(1)
  res <- monty_sample(m, sampler, n_steps, n_chains = 5, runner = runner)

  cmp <- vapply(1:5, function(i) {
    set.seed(1)
    s <- monty_sampler_random_walk(vcv = matrix(vcv[, , i], 1, 1))
    monty_sample(m, s, n_steps, n_chains = 5)$density[, i]
  }, numeric(n_steps))
  expect_equal(res$density, cmp)
})


test_that("can use the same covariance kernels on different chains", {
  m <- ex_simple_gamma1()
  runner <- monty_runner_simultaneous()
  n_steps <- 30

  set.seed(1)
  sampler1 <- monty_sampler_random_walk(vcv = array(0.01, c(1, 1, 1)))
  res1 <- monty_sample(m, sampler1, n_steps, n_chains = 5, runner = runner)

  set.seed(1)
  sampler2 <- monty_sampler_random_walk(vcv = array(0.01, c(1, 1)))
  res2 <- monty_sample(m, sampler2, n_steps, n_chains = 5, runner = runner)

  expect_equal(res1, res2)
})


test_that("can simultaneously sample a single chain", {
  m <- ex_simple_gamma1()
  vcv <- array(1 * 10^c(-4, -3, -2), c(1, 1, 3))

  runner <- monty_runner_simultaneous()
  n_steps <- 30

  set.seed(1)
  sampler1 <- monty_sampler_random_walk(vcv = vcv[, , 1, drop = FALSE])
  res1 <- monty_sample(m, sampler1, n_steps, n_chains = 1, runner = runner)

  set.seed(1)
  sampler2 <- monty_sampler_random_walk(vcv = matrix(vcv[[1]], 1, 1))
  res2 <- monty_sample(m, sampler2, n_steps, n_chains = 1, runner = runner)

  set.seed(1)
  sampler3 <- monty_sampler_random_walk(vcv = vcv)
  res3 <- monty_sample(m, sampler3, n_steps, n_chains = 3, runner = runner)

  expect_equal(res1, res2)
  expect_equal(res3$density[, 1, drop = FALSE], res1$density)
})
