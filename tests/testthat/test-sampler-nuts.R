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


test_that("nuts validates dual averaging controls", {
  expect_error(
    monty_sampler_nuts(epsilon = 0.1, target_accept = 0),
    "target_accept",
    fixed = TRUE)

  expect_error(
    monty_sampler_nuts(epsilon = 0.1, target_accept = 1),
    "target_accept",
    fixed = TRUE)

  expect_error(
    monty_sampler_nuts(epsilon = 0.1, adapt_kappa = 1.1),
    "adapt_kappa",
    fixed = TRUE)
})


test_that("nuts handles non-finite acceptance statistics during warmup", {
  m <- monty_model(list(
    parameters = "a",
    density = function(x) {
      if (all(abs(x) < 1e-12)) {
        dnorm(0, log = TRUE)
      } else {
        NaN
      }
    },
    gradient = function(x) 0
  ))
  sampler <- monty_sampler_nuts(
    epsilon = 0.1,
    warmup_steps = 1,
    adapt_step_size = TRUE)

  set.seed(1)
  res <- monty_sample(m, sampler, 1, initial = 0)

  expect_true(isTRUE(res$details$adapted))
  expect_true(is.finite(res$details$epsilon))
  expect_gt(res$details$epsilon, 0)
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


test_that("nuts is consistent with hmc and random walk on gaussian", {
  m <- monty_example("gaussian", diag(2))

  run_sampler <- function(sampler, seed) {
    set.seed(seed)
    res <- monty_sample(m, sampler, 1500)
    pars <- t(drop(res$pars))
    list(mean = colMeans(pars), cov = stats::cov(pars))
  }

  nuts <- run_sampler(monty_sampler_nuts(epsilon = 0.1, max_treedepth = 1000), 1)
  hmc <- run_sampler(monty_sampler_hmc(epsilon = 0.1, n_integration_steps = 10), 2)
  rw <- run_sampler(monty_sampler_random_walk(vcv = diag(2) * 0.1), 3)

  expect_true(all(is.finite(c(nuts$mean, hmc$mean, rw$mean))))
  expect_true(all(diag(nuts$cov) > 0.2))
  expect_true(all(diag(hmc$cov) > 0.2))
  expect_true(all(diag(rw$cov) > 0.2))

  expect_lt(max(abs(unname(nuts$mean) - unname(hmc$mean))), 1)
  expect_lt(max(abs(unname(nuts$mean) - unname(rw$mean))), 1)
})


test_that("nuts warmup can continue identically", {
  m <- monty_example("banana")
  sampler <- monty_sampler_nuts(epsilon = 0.1, max_treedepth = 1000,
                                warmup_steps = 20, adapt_step_size = TRUE)

  set.seed(1)
  res1 <- monty_sample(m, sampler, 60, restartable = TRUE)

  set.seed(1)
  res2a <- monty_sample(m, sampler, 20, restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 40, restartable = TRUE)

  expect_equal(res2b, res1)
})


test_that("nuts warmup exposes adapted epsilon details", {
  m <- monty_example("banana")
  sampler <- monty_sampler_nuts(epsilon = 0.1, max_treedepth = 1000,
                                warmup_steps = 20, adapt_step_size = TRUE)

  set.seed(1)
  res <- monty_sample(m, sampler, 40)

  expect_true(is.list(res$details))
  expect_true(is.finite(res$details$epsilon))
  expect_true(res$details$epsilon > 0)
  expect_true(isTRUE(res$details$adapted))
  expect_equal(res$details$warmup_steps, 20)
})