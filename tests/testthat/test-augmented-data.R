test_that("require data for augmented model density", {
  m <- ex_augmented()
  expect_error(
    monty_model_density(m$likelihood, c(0, 0, 0)),
    "parameters' does not have associated data, but this model requires it")
})


test_that("can run sampler with augmented data", {
  m <- ex_augmented()
  model <- m$prior + m$likelihood
  sampler <- monty_sampler_random_walk(vcv = diag(c(0.1, 0.5, 0.5)))
  res <- monty_sample(model, sampler, 10)
  expect_equal(dim(res$data), c(100, 10, 1))
})

test_that("can continue sampling with augmented_data", {
  m <- ex_augmented()
  model <- m$prior + m$likelihood
  sampler <- monty_sampler_random_walk(vcv = diag(c(0.1, 0.5, 0.5)))
  
  set.seed(1)
  res1 <- monty_sample(model, sampler, 30, n_chains = 3, restartable = TRUE)
  
  set.seed(1)
  res2a <- monty_sample(model, sampler, 10, n_chains = 3, restartable = TRUE)
  res2b <- monty_sample_continue(res2a, 20, restartable = TRUE)
  
  expect_equal(res2b, res1)
})

test_that("Can't use parallel tempering with augmented_data", {
  m <- ex_augmented()
  m$likelihood$properties$allow_multiple_parameters <- TRUE
  model <- m$prior + m$likelihood
  sampler <- monty_sampler_random_walk(vcv = diag(c(0.1, 0.5, 0.5)))
  pt_sampler <- monty_sampler_parallel_tempering(sampler, n_rungs = 10)
  
  expect_error(
    monty_sample(model, pt_sampler, 10),
    "'model' has augmented data but Parallel Tempering")
})
