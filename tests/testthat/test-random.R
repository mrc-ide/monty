## This file will eventually replace test-rng.R, but until it does we
## use the legacy interface as the source of truth.
test_that("can generate a random number", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_real(s), cmp$random_real(1))
})


test_that("can generate a random number from multiple streams at once", {
  s <- monty_random_create(seed = 42, n_streams = 10)
  cmp <- monty_rng$new(seed = 42, n_streams = 10)
  u <- monty_random_real(s)
  expect_equal(u, drop(cmp$random_real(1)))
  expect_null(dim(u))
})


test_that("can sample multiple random numbers from a single stream", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  u <- monty_random_n_real(10, s)
  expect_null(dim(u))
  expect_equal(drop(u), cmp$random_real(10))
})


test_that("can sample multiple random numbers from multiple streams", {
  s <- monty_random_create(n_streams = 3, seed = 42)
  cmp <- monty_rng$new(n_streams = 3, seed = 42)
  u <- monty_random_n_real(10, s)
  expect_equal(dim(u), c(10, 3))
  expect_equal(u, cmp$random_real(10))
})


test_that("Preserve stream dimension on generation", {
  s <- monty_random_create(n_streams = 1, seed = 42,
                           preserve_stream_dimension = TRUE)
  cmp <- monty_rng$new(n_streams = 1, seed = 42)
  u <- monty_random_n_real(10, s)
  expect_equal(dim(u), c(10, 1))
  expect_equal(u, cbind(cmp$random_real(10)))

  u <- monty_random_n_real(1, s)
  expect_equal(dim(u), c(1, 1))
  expect_equal(u, cbind(cmp$random_real(1)))
})


test_that("can get and set rng state", {
  s <- monty_random_create(seed = 42, n_streams = 10)
  cmp <- monty_rng$new(seed = 42, n_streams = 10)
  r <- monty_random_state(s)
  expect_type(r, "raw")
  expect_length(r, 320)
  expect_equal(r, cmp$state())

  monty_random_set_state(rev(r), s)
  cmp$set_state(rev(r))
  expect_equal(monty_random_state(s), rev(r))
  expect_equal(monty_random_real(s), drop(cmp$random_real(1)))
})


test_that("can sample from exponential distribution (rate)", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_exponential_rate(0.5, s),
               cmp$exponential_rate(1, 0.5))
  expect_equal(monty_random_n_exponential_rate(10, 0.5, s),
               cmp$exponential_rate(10, 0.5))
})


test_that("can sample from binomial distribution", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_binomial(10, 0.3, s),
               cmp$binomial(1, 10, 0.3))
  expect_equal(monty_random_n_binomial(10, 7, 0.3, s),
               cmp$binomial(10, 7, 0.3))
})
