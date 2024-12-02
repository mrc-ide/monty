test_that("can validate sample inputs", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_error(monty_sample(NULL, NULL, 100),
               "Expected 'model' to be a 'monty_model'")
  expect_error(monty_sample(model, NULL, 100),
               "Expected 'sampler' to be a 'monty_sampler'")
  expect_error(monty_sample(model, sampler, 100, runner = TRUE),
               "Expected 'runner' to be a 'monty_runner'")
  expect_error(monty_sample(model, sampler, 100, c(1, 2)),
               "Unexpected length for vector 'initial' (given 2, expected 1)",
               fixed = TRUE)
})


test_that("sampler return value contains history", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  res <- monty_sample(model, sampler, 100, 1)
  ## TODO: what is in details?
  expect_setequal(names(res),
                  c("pars", "density", "initial", "details", "observations"))
  expect_equal(dim(res$pars), c(1, 100, 1))
  expect_equal(dimnames(res$pars), list("gamma", NULL, NULL))
  expect_equal(dim(res$density), c(100, 1))
  expect_equal(drop(res$density), drop(apply(res$pars, 1:2, model$density)))
  expect_equal(res$initial, rbind(gamma = 1))
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
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_no_warning(
    monty_sample(model1, sampler, 100))
  expect_warning(
    monty_sample(model2, sampler, 100),
    "Detected use of R's random number generators")
})


test_that("sample initial state if not provided, with a single chain", {
  m <- ex_simple_gamma1()
  g1 <- monty_rng_create(seed = 42)
  g2 <- monty_rng_create(seed = 42)

  res <- initial_parameters(NULL, m, list(g1))
  expect_equal(res, matrix(m$direct_sample(g2), 1, 1))
})


test_that("sample initial state if not provided, with multiple chains", {
  m <- ex_simple_gamma1()
  g1 <- initial_rng(3, 42)
  g21 <- monty_rng_create(seed = 42)
  g22 <- monty_rng_long_jump(monty_rng_create(seed = 42))
  g23 <- monty_rng_long_jump(monty_rng_create(seed = 42), 2)

  res <- initial_parameters(NULL, m, g1)
  expect_equal(dim(res), c(1, 3))
  expect_equal(res[1, 1], m$direct_sample(g21))
  expect_equal(res[1, 2], m$direct_sample(g22))
  expect_equal(res[1, 3], m$direct_sample(g23))
})


test_that("validate that initial have correct size for list inputs", {
  m <- ex_simple_gamma1()
  r <- initial_rng(2)
  expect_equal(
    initial_parameters(list(1, 2), m, r),
    rbind(c(1, 2)))
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
    initial_parameters(matrix(c(1, 2), 1, 2), m, r),
    rbind(c(1, 2)))
  expect_error(
    initial_parameters(matrix(1, 2, 2), m, r),
    "Unexpected number of rows in 'initial' (given 2, expected 1)",
    fixed = TRUE)
  expect_error(
    initial_parameters(matrix(1, 1, 3), m, r),
    "Unexpected number of columns in 'initial' (given 3, expected 2)",
    fixed = TRUE)
})


test_that("validate that initial have correct size for vector inputs", {
  m <- ex_simple_gamma1()
  r <- initial_rng(2)
  expect_equal(
    initial_parameters(1, m, r),
    rbind(c(1, 1)))
  expect_error(
    initial_parameters(c(1, 2), m, r),
    "Unexpected length for vector 'initial' (given 2, expected 1)",
    fixed = TRUE)
})


test_that("sample from previous samples", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  set.seed(1)
  samples <- monty_sample(model, sampler, 100, 1, n_chains = 2)
  r1 <- initial_rng(6, seed = 42)

  initial <- initial_parameters(samples, model, r1)
  expect_equal(dim(initial), c(1, 6))

  cmp <- tail_and_pool(samples$pars, 0.05, 20)
  r2 <- initial_rng(6, seed = 42)
  i <- ceiling(vnapply(r2, function(r) monty_random_real(r)) * ncol(cmp))
  expect_equal(initial, cmp[, i, drop = FALSE])
})


test_that("validate parameter size when using previous samples", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  set.seed(1)
  samples <- monty_sample(model, sampler, 100, 1, n_chains = 2)

  samples$pars <- array(samples$pars, c(3, 50, 2))
  r <- initial_rng(6, seed = 42)
  expect_error(
    initial_parameters(samples, model, r),
    "Unexpected parameter length in 'monty_samples' object 'initial'")
})


test_that("can run more than one chain, in parallel", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 1, n_chains = 2)

  set.seed(1)
  res2 <- monty_sample(model, sampler, 100, 1, n_chains = 2,
                         runner = monty_runner_parallel(2))

  expect_identical(res1, res2)
})


test_that("need a direct sample function in order to start sampling", {
  model1 <- ex_simple_gamma1()
  model2 <- monty_model(list(density = model1$density,
                               parameters = model1$parameters,
                               domain = model1$domain))
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_no_error(
    monty_sample(model1, sampler, 10))
  expect_error(
    monty_sample(model2, sampler, 10),
    "'initial' must be provided with this model")
})


test_that("can continue chains", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 1, n_chains = 3)

  set.seed(1)
  res2a <- monty_sample(model, sampler, 50, 1, n_chains = 3,
                          restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 50)

  expect_equal(res2b, res1)
})


test_that("can continue continuable chains", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 30, 1, n_chains = 3,
                         restartable = TRUE)

  set.seed(1)
  res2a <- monty_sample(model, sampler, 10, 1, n_chains = 3,
                          restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 10, restartable = TRUE)
  res2c <- monty_sample_continue(res2b, 10, restartable = TRUE)

  expect_equal(res2c, res1)
})


test_that("can't restart chains that don't have restart information", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  res <- monty_sample(model, sampler, 5, 1, n_chains = 3)
  expect_error(monty_sample_continue(res, 50),
               "Your chains are not restartable")
})


test_that("continuing requires that we have a samples object", {
  model <- ex_simple_gamma1()
  expect_error(monty_sample_continue(model, 50),
               "Expected 'samples' to be a 'monty_samples' object")
})


test_that("generate initial conditions that fall within the domain", {
  x <- monty_random_n_normal(20, -2, 1, monty_rng_create(seed = 1))
  n <- which(x > 0)[1] # 5

  m <- monty_model(list(
    parameters = "x",
    direct_sample = function(rng) rng$normal(1, -2, 1),
    density = function(x) dexp(x, log = TRUE),
    domain = rbind(c(0, Inf))))

  r <- monty_rng_create(seed = 1)
  expect_equal(direct_sample_within_domain(m, r), x[n])

  r <- monty_rng_create(seed = 1)
  expect_error(
    direct_sample_within_domain(m, r, n - 1),
    "Failed to sample initial conditions within \\d+ attempts")
})


test_that("error if provided initial conditions fall outside of domain", {
  m <- monty_model(list(
    parameters = c("x", "y", "z"),
    density = identity,
    domain = rbind(c(0, Inf),
                   c(0, 1),
                   c(-Inf, Inf))))
  sampler <- monty_sampler_random_walk(vcv = diag(3) * 0.01)

  err <- expect_error(
    monty_sample(m, sampler, 100, c(-1, 0, 0)),
    "Initial conditions do not fall within parameter domain")
  expect_equal(
    err$body,
    c(x = "Issues with parameter: 'x'",
      x = "Issues with chain 1"))

  err <- expect_error(
    monty_sample(m, sampler, 100, c(-1, 0, 0), n_chains = 2),
    "Initial conditions do not fall within parameter domain")
  expect_equal(
    err$body,
    c(x = "Issues with parameter: 'x'",
      x = "Issues with every chain"))

  err <- expect_error(
    monty_sample(m, sampler, 100,
                   cbind(c(-1, 0, 0), c(-1, -1, 0), c(0, 0, 0)), n_chains = 3),
    "Initial conditions do not fall within parameter domain")
  expect_equal(
    err$body,
    c(x = "Issues with parameters: 'x' and 'y'",
      x = "Issues with chains 1 and 2"))
})


test_that("error if initial conditions do not have finite density", {
  m <- monty_model(list(
    parameters = "x",
    direct_sample = function(rng) 1,
    density = function(x) -Inf,
    domain = rbind(c(-Inf, Inf))))
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  expect_error(monty_sample(m, sampler, 100),
               "Chain does not have finite starting density")
})


test_that("can continue a stochastic model identically", {
  set.seed(1)
  model <- ex_sir_filter_posterior()
  vcv <- matrix(c(0.0006405, 0.0005628, 0.0005628, 0.0006641), 2, 2)
  sampler <- monty_sampler_random_walk(vcv = vcv)
  initial <- c(0.2, 0.1)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 10, initial, n_chains = 2)

  set.seed(1)
  res2a <- monty_sample(model, sampler, 2, initial, n_chains = 2,
                          restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 8)

  expect_equal(res2b, res1)
})


test_that("can continue parallel runs", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 1, n_chains = 3)

  set.seed(1)
  runner <- monty_runner_parallel(2)
  res2a <- monty_sample(model, sampler, 50, 1, n_chains = 3,
                          runner = runner, restartable = TRUE)
  expect_identical(res2a$restart$runner, runner)
  res2b <- monty_sample_continue(res2a, 50)

  expect_equal(res2b, res1)
})


test_that("can change runner on restart", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 1, n_chains = 3)

  runner_parallel <- monty_runner_parallel(2)
  runner_serial <- monty_runner_serial()

  set.seed(1)
  res2a <- monty_sample(model, sampler, 20, 1, n_chains = 3,
                         restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 30, restartable = TRUE,
                                   runner = runner_parallel)
  res2c <- monty_sample_continue(res2b, 40, restartable = TRUE,
                                   runner = runner_serial)
  res2d <- monty_sample_continue(res2c, 10)

  expect_identical(res2b$restart$runner, runner_parallel)
  expect_identical(res2c$restart$runner, runner_serial)

  expect_equal(res2d, res1)
})


test_that("validate burnin", {
  expect_equal(monty_sample_steps(100, NULL)$burnin, 0)
  expect_equal(monty_sample_steps(100, 0)$burnin, 0)
  expect_equal(monty_sample_steps(100, 20)$burnin, 20)
  expect_error(monty_sample_steps(100, 200),
               "'burnin' must be smaller than 'n_steps'")
})


test_that("validate thinning rate", {
  expect_equal(
    monty_sample_steps(100, thinning_factor = NULL)$thinning_factor, 1)
  expect_equal(
    monty_sample_steps(100, thinning_factor = 1)$thinning_factor, 1)
  expect_equal(
    monty_sample_steps(100, thinning_factor = 20)$thinning_factor, 20)
  expect_error(
    monty_sample_steps(100, thinning_factor = 0),
    "'thinning_factor' must be at least 1")
  expect_error(
    monty_sample_steps(100, thinning_factor = 17)$thinning_factor,
    "'thinning_factor' must be a divisor of 'n_steps'")
})


test_that("can sample with burnin", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 3)
  set.seed(1)
  res2 <- monty_sample(model, sampler, 100, 3, burnin = 30)
  expect_equal(res2$pars, res1$pars[, 31:100, , drop = FALSE])
})


test_that("can sample with thinning", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 3)
  set.seed(1)
  res2 <- monty_sample(model, sampler, 100, 3, thinning_factor = 4)
  expect_equal(res2$pars, res1$pars[, seq(4, 100, by = 4), , drop = FALSE])
})


test_that("can sample with burnin and thinning", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 3)
  ## Pick deliberately hard values here:
  set.seed(1)
  res2 <- monty_sample(model, sampler, 100, 3,
                       burnin = 25, thinning_factor = 10)
  expect_equal(res2$pars, res1$pars[, seq(30, 100, by = 10), , drop = FALSE])
  set.seed(1)
  res3 <- monty_sample(model, sampler, 100, 3,
                       burnin = 20, thinning_factor = 10)
  expect_equal(res3$pars, res1$pars[, seq(30, 100, by = 10), , drop = FALSE])
})


test_that("can continue burnt in chain, does not burnin any further", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 1, n_chains = 3, burnin = 20)

  set.seed(1)
  res2a <- monty_sample(model, sampler, 60, 1, n_chains = 3,
                        burnin = 20, restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 40)

  expect_equal(res2b, res1)
})


test_that("can continue thinned chain, continues thinning", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 1, n_chains = 3,
                       thinning_factor = 4)

  set.seed(1)
  res2a <- monty_sample(model, sampler, 60, 1, n_chains = 3,
                        thinning_factor = 4, restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 40)

  expect_equal(res2b, res1)
})


test_that("can choose not to append when continuing samples", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1 <- monty_sample(model, sampler, 100, 1, n_chains = 3)

  set.seed(1)
  res2a <- monty_sample(model, sampler, 60, 1, n_chains = 3,
                          restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 40, append = FALSE)

  expect_equal(res2b$pars, res1$pars[, 61:100, , drop = FALSE])
})


test_that("can use samples as initial conditions", {
  set.seed(1)
  m <- ex_sir_filter_posterior(n_particles = 20)
  vcv <- matrix(c(0.0006405, 0.0005628, 0.0005628, 0.0006641), 2, 2)
  sampler <- monty_sampler_random_walk(vcv = vcv)
  initial <- c(0.2, 0.1)

  set.seed(1)
  res1 <- monty_sample(m, sampler, 50, initial, n_chains = 2)
  res2 <- monty_sample(m, sampler, 20, res1, n_chains = 3)

  expect_equal(dim(res1$pars), c(2, 50, 2))
  expect_equal(dim(res2$pars), c(2, 20, 3))
})
