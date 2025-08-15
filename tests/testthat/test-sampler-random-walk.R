test_that("can draw samples from a trivial model", {
  m <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = matrix(0.01, 1, 1))
  res <- monty_sample(m, sampler, 100)

  expect_equal(
    names(res),
    c("pars", "density", "initial", "details", "state", "observations"))
  expect_equal(dim(res$pars), c(1, 100, 1))
})


test_that("validate sampler against model on initialisation", {
  m <- ex_simple_gamma1()

  state <- list(pars = 1, density = -Inf)
  sampler1 <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  sampler2 <- monty_sampler_random_walk(vcv = diag(2) * 0.01)
  r <- monty_rng_create()

  expect_no_error(sampler1$initialise(state, sampler1$control, m, r))
  expect_error(
    sampler2$initialise(state, sampler2$control, m, r),
    "Incompatible length parameters (1) and vcv (2)",
    fixed = TRUE)
})


test_that("can draw samples from a random model", {
  set.seed(1)
  m <- ex_sir_filter_posterior()
  vcv <- matrix(c(0.0006405, 0.0005628, 0.0005628, 0.0006641), 2, 2)
  sampler <- monty_sampler_random_walk(vcv = vcv)
  res <- monty_sample(m, sampler, 20)
  expect_setequal(
    names(res),
    c("pars", "density", "initial", "details", "state", "observations"))
})


test_that("can observe a model", {
  m <- ex_sir_filter_posterior(save_trajectories = TRUE)
  vcv <- matrix(c(0.0006405, 0.0005628, 0.0005628, 0.0006641), 2, 2)
  sampler <- monty_sampler_random_walk(vcv = vcv)

  ## This takes quite a while, and that seems mostly to be the time
  ## taken to call the filter in dust.
  res <- monty_sample(m, sampler, 20, n_chains = 3)
  expect_setequal(
    names(res),
    c("pars", "density", "initial", "details", "state", "observations"))
  expect_equal(names(res$observations),
               "trajectories")
  expect_equal(dim(res$observations$trajectories),
               c(2, 9, 20, 3)) # states, time steps, samples, chains
})


test_that("can continue observed models", {
  m <- ex_sir_filter_posterior(save_trajectories = TRUE)
  vcv <- matrix(c(0.0006405, 0.0005628, 0.0005628, 0.0006641), 2, 2)
  sampler <- monty_sampler_random_walk(vcv = vcv)

  set.seed(1)
  res1 <- monty_sample(m, sampler, 15, n_chains = 3)

  set.seed(1)
  res2a <- monty_sample(m, sampler, 5, n_chains = 3, restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 10)

  expect_equal(res1$observations, res2b$observations)
})


test_that("can run multiple samples at once", {
  m <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = matrix(0.01, 1, 1))
  p <- matrix(runif(5), 1)
  state0 <- list(pars = p, density = m$density(p))
  ## TODO: we need a much better rng support here; we'll need to make
  ## a tweak to the rng code to to a long jump between each chain.
  r <- monty_rng_create(n_streams = 5)
  state_sampler <- sampler$initialise(state0, sampler$control, m, r)
  state1 <- sampler$step(state0, state_sampler, sampler$control, m, r)

  expect_equal(dim2(state0$pars), c(1, 5))
  expect_equal(dim2(state1$pars), c(1, 5))
  expect_equal(dim2(state0$density), 5)
  expect_equal(dim2(state1$density), 5)
})


test_that("can run random walk sampler simultaneously", {
  m <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = matrix(0.01, 1, 1))

  set.seed(1)
  res1 <- monty_sample(m, sampler, 100, n_chains = 3)

  set.seed(1)
  runner <- monty_runner_simultaneous()
  res2 <- monty_sample(m, sampler, 100, n_chains = 3, runner = runner)
  expect_equal(res1, res2)
})


test_that("can continue a simultaneous random walk sampler", {
  m <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = matrix(0.01, 1, 1))

  set.seed(1)
  res1a <- monty_sample(m, sampler, 30, n_chains = 3, restartable = TRUE)
  res1b <- monty_sample_continue(res1a, 70)

  set.seed(1)
  runner <- monty_runner_simultaneous()
  res2a <- monty_sample(m, sampler, 30, n_chains = 3, runner = runner,
                        restartable = TRUE)

  expect_equal(drop_runner(res2a), drop_runner(res1a))

  res2b <- monty_sample_continue(res2a, 70)

  expect_equal(res2b, res1b)
})


test_that("validate number of parameter sets", {
  m <- ex_simple_gamma1()
  vcv <- array(1 * 10^c(-4, -3, -2, -1, 0), c(1, 1, 5))
  sampler <- monty_sampler_random_walk(vcv = vcv)
  expect_error(
    monty_sample(m, sampler, 200, n_chains = 5),
    "Incompatible number of parameter sets (1) and slices in vcv (5)",
    fixed = TRUE)
  runner <- monty_runner_simultaneous()
  expect_error(
    monty_sample(m, sampler, 200, n_chains = 3, runner = runner),
    "Incompatible number of parameter sets (3) and slices in vcv (5)",
    fixed = TRUE)
})


test_that("can run sampler with reflecting boundaries", {
  model <- monty_model(
    list(parameters = "x",
         domain = cbind(-1, 1),
         density = function(x) {
           if (abs(x) > 1) {
             stop("parameter out of bounds")
           }
           0.5
         },
         direct_sample = function(rng) {
           monty_random_uniform(-1, 1, rng)
         }))

  s1 <- monty_sampler_random_walk(matrix(0.5, 1, 1), boundaries = "ignore")
  s2 <- monty_sampler_random_walk(matrix(0.5, 1, 1), boundaries = "reflect")
  s3 <- monty_sampler_random_walk(matrix(0.5, 1, 1), boundaries = "reject")

  expect_error(monty_sample(model, s1, 100), "parameter out of bounds")

  res2 <- monty_sample(model, s2, 100)
  r2 <- range(drop(res2$pars))
  expect_gt(diff(r2), 0.75)
  expect_gt(r2[[1]], -1)
  expect_lt(r2[[2]], 1)

  res3 <- monty_sample(model, s3, 100)
  r3 <- range(drop(res3$pars))
  expect_gt(diff(r3), 0.75)
  expect_gt(r3[[1]], -1)
  expect_lt(r3[[2]], 1)

  ## Different with rejection than reflection, and more step
  ## rejections when rejection used.
  expect_true(!all(res2$pars == res3$pars))
  expect_gt(sum(diff(drop(res3$pars)) == 0),
            sum(diff(drop(res2$pars)) == 0))
})


test_that("can run sampler with rejecting boundaries", {
  model <- monty_model(
    list(parameters = "x",
         domain = cbind(-1, 1),
         density = function(x) {
           if (abs(x) > 1) {
             stop("parameter out of bounds")
           }
           0.5
         },
         direct_sample = function(rng) {
           monty_random_uniform(-1, 1, rng)
         }))

  s1 <- monty_sampler_random_walk(matrix(0.5, 1, 1), boundaries = "ignore")
  s2 <- monty_sampler_random_walk(matrix(0.5, 1, 1), boundaries = "reject")

  expect_error(monty_sample(model, s1, 100), "parameter out of bounds")
  res <- monty_sample(model, s2, 100)
  r <- range(drop(res$pars))
  expect_gt(diff(r), 0.75)
  expect_gt(r[[1]], -1)
  expect_lt(r[[2]], 1)
})


test_that("can run sampler with rejecting boundaries simultaneously", {
  m <- monty_model(
    list(parameters = "x",
         domain = cbind(-1, 1),
         density = function(x) {
           ifelse(abs(x) > 1, NA_real_, log(0.5))
         },
         direct_sample = function(rng) {
           monty_random_uniform(-1, 1, rng)
         }),
    monty_model_properties(allow_multiple_parameters = TRUE))

  s <- monty_sampler_random_walk(matrix(0.5, 1, 1), boundaries = "reject")
  runner <- monty_runner_simultaneous()

  n_steps <- 30

  set.seed(1)
  res <- monty_sample(m, s, n_steps, n_chains = 4, runner = runner)
  set.seed(1)
  cmp <- monty_sample(m, s, n_steps, n_chains = 4)

  expect_equal(res, cmp)
})


test_that("can print a sampler object", {
  s <- monty_sampler_random_walk(diag(1))
  res <- evaluate_promise(withVisible(print(s)))
  expect_mapequal(res$result, list(value = s, visible = FALSE))
  expect_match(
    res$messages,
    "<monty_sampler: Random walk (monty_random_walk)>",
    fixed = TRUE, all = FALSE)
})


test_that("Can rerun a stochastic model", {
  set.seed(1)
  m <- ex_sir_filter_posterior()
  vcv <- matrix(c(0.0006405, 0.0005628, 0.0005628, 0.0006641), 2, 2)
  sampler1 <- monty_sampler_random_walk(vcv = vcv, rerun_every = 2,
                                        rerun_random = FALSE)
  sampler2 <- monty_sampler_random_walk(vcv = vcv, rerun_every = 2)
  res1 <- monty_sample(m, sampler1, 20)
  res2 <- monty_sample(m, sampler2, 20)

  change_density1 <- diff(res1$density) != 0
  change_pars1 <- diff(res1$pars[1, , 1]) != 0

  change_density2 <- diff(res2$density) != 0
  change_pars2 <- diff(res2$pars[1, , 1]) != 0

  expect_gt(sum(change_density1), sum(change_pars1))
  expect_gt(sum(change_density2), sum(change_pars2))

  expect_true(all(change_density1[seq(1, 20, by = 2)]))
  expect_false(all(change_density2[seq(1, 20, by = 2)]))
})


test_that("can continue periodic rerun models", {
  set.seed(1)
  m <- ex_sir_filter_posterior()
  vcv <- matrix(c(0.0006405, 0.0005628, 0.0005628, 0.0006641), 2, 2)
  sampler <- monty_sampler_random_walk(vcv = vcv, rerun_every = 7,
                                       rerun_random = FALSE)

  set.seed(1)
  res1 <- monty_sample(m, sampler, 30, n_chains = 2)

  set.seed(1)
  res2a <- monty_sample(m, sampler, 10, n_chains = 2, restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 20)

  expect_equal(res1, res2b)
})
