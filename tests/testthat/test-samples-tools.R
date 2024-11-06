test_that("can thin chain", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  res <- monty_sample(model, sampler, 500, 1)

  expect_identical(monty_samples_thin(res), res)
  expect_identical(monty_samples_thin(res, 0, 0), res)

  res1 <- monty_samples_thin(res, burnin = 100)
  expect_equal(res1$pars, res$pars[, -(1:100), , drop = FALSE])
  expect_equal(res1$density, res$density[-(1:100), , drop = FALSE])
  expect_equal(res1, monty_samples_subset(res, 101:500))

  res2 <- monty_samples_thin(res, thinning_factor = 10)
  i <- seq(10, 500, by = 10)
  expect_equal(res2, monty_samples_subset(res, i))

  res3 <- monty_samples_thin(res, burnin = 123, thinning_factor = 17)
  i <- seq(126, 500, by = 17)
  expect_equal(res3, monty_samples_subset(res, i))
})


test_that("can't discard whole chain as burnin", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  res <- monty_sample(model, sampler, 10, 1)
  expect_error(
    monty_samples_thin(res, burnin = 100),
    "'burnin' must be smaller than 10 for your samples")
  expect_error(
    monty_samples_thin(res, burnin = 10),
    "'burnin' must be smaller than 10 for your samples")
})


test_that("can subset chains with observations", {
  m <- ex_sir_filter_posterior(save_trajectories = TRUE)
  vcv <- matrix(c(0.0006405, 0.0005628, 0.0005628, 0.0006641), 2, 2)
  sampler <- monty_sampler_random_walk(vcv = vcv)

  ## This takes quite a while, and that seems mostly to be the time
  ## taken to call the filter in dust.
  res <- monty_sample(m, sampler, 20, n_chains = 3)

  i <- c(4, 7, 8)
  res1 <- monty_samples_subset(res, i)

  expect_equal(names(res1), names(res))
  expect_equal(res1$pars, res$pars[, i, ])
  expect_equal(res1$density, res$density[i, ])
  expect_equal(names(res1$observations), "trajectories")
  expect_equal(res1$observations$trajectories,
               res$observations$trajectories[, , i, ])
})


test_that("warn if samples contain unsubsettalble data", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  samples <- monty_sample(model, sampler, 500, 1, n_chains = 3)
  samples$observations <- list(a = 1,
                               b = array(1:3000, c(3, 500, 2)),
                               c = array(1:3000, c(2, 500, 3)))
  expect_warning(
    res <- monty_samples_thin(samples, burnin = 100, thinning_factor = 10),
    "Cannot subset 'observations[[\"a\"]]' and 'observations[[\"b\"]]'",
    fixed = TRUE)

  i <- seq(110, 500, by = 10)
  expect_equal(res$pars, samples$pars[, i, , drop = FALSE])
  expect_equal(res$density, samples$density[i, , drop = FALSE])
  expect_equal(res$observations$a, samples$observations$a)
  expect_equal(res$observations$b, samples$observations$b)
  expect_equal(res$observations$c, samples$observations$c[, i, , drop = FALSE])
})
