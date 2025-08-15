test_that("can run a PT sampler", {
  set.seed(1)
  likelihood <- ex_mixture(5)
  prior <- monty_dsl({
    x ~ Normal(0, 10)
  })
  posterior <- likelihood + prior
  sampler <- monty_sampler_parallel_tempering(
    n_rungs = 10,
    sampler = monty_sampler_random_walk(vcv = matrix(0.1)))
  res <- monty_sample(posterior, sampler, 50, n_chains = 4)

  expect_type(res$details, "list")
  expect_setequal(names(res$details), c("accept_swap", "sampler"))
  expect_equal(dim(res$details$accept_swap), c(10, 4))
  expect_null(res$details$sampler)
})


test_that("can sample with base model", {
  likelihood <- ex_mixture(5)
  prior <- monty_dsl({
    x ~ Normal(0, 3)
  })
  base <- monty_dsl({
    x ~ Normal(0, 10)
  })

  posterior <- likelihood + prior
  sampler <- monty_sampler_parallel_tempering(
    n_rungs = 10,
    sampler = monty_sampler_random_walk(vcv = matrix(0.1)),
    base = base)

  expect_identical(sampler$control$base, base)

  res <- monty_sample(posterior, sampler, 100, n_chains = 4)

  expect_type(res$details, "list")
  expect_setequal(names(res$details), c("accept_swap", "sampler"))
  expect_equal(dim(res$details$accept_swap), c(10, 4))
  expect_null(res$details$sampler)
})


test_that("require that base model is suitable", {
  sampler <- monty_sampler_random_walk(vcv = matrix(0.1))
  expect_error(
    monty_sampler_parallel_tempering(n_rungs = 10, sampler = sampler,
                                     base = TRUE),
    "Expected 'base' to be a 'monty_model' object")

  base <- monty_model(list(parameters = "x", density = identity))
  expect_error(
    monty_sampler_parallel_tempering(n_rungs = 10, sampler = sampler,
                                     base = base),
    "Can't use 'base' as a base model")

  base <- monty_model(list(parameters = "x",
                           density = identity,
                           direct_sample = identity))
  expect_error(
    monty_sampler_parallel_tempering(n_rungs = 10, sampler = sampler,
                                     base = base),
    "Can't use 'base' as a base model")
})


test_that("base model and target must have same parameters", {
  base <- monty_dsl({
    x ~ Normal(0, 10)
  })
  target <- monty_dsl({
    y ~ Normal(0, 10)
  })
  s <- monty_sampler_parallel_tempering(
    n_rungs = 10,
    sampler = monty_sampler_random_walk(vcv = matrix(0.1)),
    base = base)
  expect_error(
    monty_sample(target, s, 100),
    "'base' and 'model' must have the same parameters")
})


## This tests get/set of the internal state
test_that("can continue a parallel tempering chain", {
  likelihood <- ex_mixture(5)
  prior <- monty_dsl({
    x ~ Normal(0, 10)
  })
  posterior <- likelihood + prior

  set.seed(1)
  sampler <- monty_sampler_parallel_tempering(
    n_rungs = 10,
    sampler = monty_sampler_random_walk(vcv = matrix(0.1)))
  res1 <- monty_sample(posterior, sampler, 30, n_chains = 4, restartable = TRUE)
  res2 <- monty_sample_continue(res1, 70)

  set.seed(1)
  sampler <- monty_sampler_parallel_tempering(
    n_rungs = 10,
    sampler = monty_sampler_random_walk(vcv = matrix(0.1)))
  cmp <- monty_sample(posterior, sampler, 100, n_chains = 4)

  expect_equal(res2$pars, cmp$pars)
})


test_that("can validate beta", {
  expect_equal(validate_parallel_tempering_beta(10, NULL),
               seq(1, 0, length.out = 11))
  expect_equal(validate_parallel_tempering_beta(10, seq(1, 0, length.out = 11)),
               seq(1, 0, length.out = 11))

  expect_error(validate_parallel_tempering_beta(NULL, NULL),
               "One of 'n_rungs' or 'beta' must be provided")
  expect_error(validate_parallel_tempering_beta(5, 1:4),
               "Only one of 'n_rungs' or 'beta' may be provided")

  expect_error(validate_parallel_tempering_beta(0, NULL),
               "'n_rungs' must be at least 1")

  expect_error(validate_parallel_tempering_beta(NULL, 1),
               "At least two 'beta' values are required")
  expect_error(validate_parallel_tempering_beta(NULL, seq(0, 1, 0.1)),
               "'beta[1]' must be 1", fixed = TRUE)
  expect_error(validate_parallel_tempering_beta(NULL, c(1, 0.5, 0.2)),
               "'beta[3]' (the last value) must be 0", fixed = TRUE)
  expect_error(validate_parallel_tempering_beta(NULL, c(1, 0.5, 0.5, 0)),
               "'beta' must be strictly decreasing, with no ties")
})


test_that("prevent use of the simultaneous runner with parallel tempering", {
  m <- ex_simple_gamma1()
  sampler <- monty_sampler_parallel_tempering(
    n_rungs = 10,
    sampler = monty_sampler_random_walk(vcv = matrix(0.1)))
  runner <- monty_runner_simultaneous()
  expect_error(
    monty_sample(m, sampler, 100, n_chains = 3, runner = runner),
    "Can't use the simultaneous runner with this sampler")
})
