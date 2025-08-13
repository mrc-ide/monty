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
  res <- monty_sample(posterior, sampler, 100, n_chains = 4)

  skip("update state")
  expect_type(res$details, "list")
  expect_equal(names(res$details[[1]]), "accept_swap")
  expect_length(res$details[[1]]$accept_swap, 10)
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

  skip("update state")
  expect_type(res$details, "list")
  expect_equal(names(res$details[[1]]), "accept_swap")
  expect_length(res$details[[1]]$accept_swap, 10)
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
  skip("rewrite state")
  likelihood <- ex_mixture(5)
  prior <- monty_dsl({
    x ~ Normal(0, 10)
  })
  posterior <- likelihood + prior

  set.seed(1)
  s <- monty_sampler_parallel_tempering(n_rungs = 10, vcv = matrix(0.1))
  res1 <- monty_sample(posterior, s, 30, n_chains = 4, restartable = TRUE)
  res2 <- monty_sample_continue(res1, 70)

  set.seed(1)
  s <- monty_sampler_parallel_tempering(n_rungs = 10, vcv = matrix(0.1))
  cmp <- monty_sample(posterior, s, 100, n_chains = 4)

  expect_equal(res2$pars, cmp$pars)
})
