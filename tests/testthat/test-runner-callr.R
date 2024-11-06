test_that("can run a chain in parallel via callr", {
  m <- monty_example("banana")
  s <- monty_sampler_random_walk(vcv = diag(2) * 0.02)
  r <- monty_runner_callr(2)
  set.seed(1)
  res1 <- monty_sample(m, s, 100, n_chains = 4)

  set.seed(1)
  res2 <- monty_sample(m, s, 100, n_chains = 4, runner = r)

  expect_equal(res1, res2)
})


test_that("can continue parallel runs", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)

  set.seed(1)
  res1a <- monty_sample(model, sampler, 50, 1, n_chains = 3,
                        restartable = TRUE)
  res1b <- monty_sample_continue(res1a, 50)

  set.seed(1)
  runner <- monty_runner_callr(2)
  res2a <- monty_sample(model, sampler, 50, 1, n_chains = 3,
                        runner = runner, restartable = TRUE)
  expect_identical(res2a$restart$runner, runner)
  res2b <- monty_sample_continue(res2a, 50)

  expect_equal(res2b, res1b)
})


test_that("can produce progress bars", {
  m <- monty_example("banana")
  s <- monty_sampler_random_walk(vcv = diag(2) * 0.02)
  r <- monty_runner_callr(2, progress = TRUE)
  res <- evaluate_promise(monty_sample(m, s, 100, n_chains = 4, runner = r))
  expect_match(res$messages, "Sampled 400 steps across 4 chains in",
               all = FALSE)
})
