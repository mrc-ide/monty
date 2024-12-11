test_that("can print rng state", {
  s <- monty_rng_create()
  res <- evaluate_promise(withVisible(print(s)))
  expect_equal(res$result,
               list(value = s, visible = FALSE))
  expect_match(res$messages, "<monty_rng_state>", fixed = TRUE, all = FALSE)
  expect_match(res$messages, "1 random number stream", all = FALSE)
  expect_match(res$messages, "1 execution thread", all = FALSE)
})


test_that("can get the length of rng state as the number of streams", {
  expect_equal(length(monty_rng_create()), 1)
  expect_equal(length(monty_rng_create(n_streams = 3)), 3)
})


test_that("Preserve stream dimension on generation", {
  s <- monty_rng_create(n_streams = 1, seed = 42,
                           preserve_stream_dimension = TRUE)
  cmp <- monty_rng_create(n_streams = 1, seed = 42)
  u <- monty_random_n_real(10, s)
  expect_equal(dim(u), c(10, 1))
  expect_equal(u, cbind(monty_random_n_real(10, cmp)))

  u <- monty_random_n_real(1, s)
  expect_equal(dim(u), c(1, 1))
  expect_equal(u, cbind(monty_random_n_real(1, cmp)))
})


test_that("can get and set rng state", {
  s <- monty_rng_create(seed = 42, n_streams = 10)
  r <- monty_rng_state(s)
  expect_type(r, "raw")
  expect_length(r, 320)

  monty_rng_set_state(rev(r), s)
  expect_equal(monty_rng_state(s), rev(r))

  expect_error(
    monty_rng_set_state(r[-1], s),
    "'value' must be a raw vector of length 320 (but was 319)",
    fixed = TRUE)
})


test_that("can jump", {
  r1 <- monty_rng_create(n_streams = 1, seed = 1)
  r2 <- monty_rng_create(n_streams = 5, seed = 1)
  s1 <- monty_rng_state(r1)
  s2 <- matrix(monty_rng_state(r2), ncol = 5)
  expect_equal(monty_rng_state(r1), s2[, 1])
  monty_rng_jump(r1)
  expect_equal(monty_rng_state(r1), s2[, 2])
  monty_rng_jump(r1, 3)
  expect_equal(monty_rng_state(r1), s2[, 5])
  expect_equal(monty_rng_jump(s1), s2[, 2])
  expect_equal(monty_rng_jump(s1, 4), s2[, 5])
})


## This test is not great, but it will end up done properly once we
## replace things in the reference tests and harmonise the pointer
## interface with this new one.
test_that("Can long jump", {
  r <- monty_rng_create(seed = 1L)
  s <- monty_rng_distributed_state(seed = 1L, n_nodes = 5)
  expect_equal(monty_rng_state(r), s[[1]])
  monty_rng_long_jump(r)
  expect_equal(monty_rng_state(r), s[[2]])
  expect_equal(monty_rng_long_jump(s[[1]]), s[[2]])
  expect_equal(monty_rng_long_jump(s[[1]], 4), s[[5]])
})


test_that("Can vary parameters across streams", {
  s <- monty_rng_create(seed = 42, n_streams = 3)
  r <- matrix(monty_rng_state(s), ncol = 3)
  cmp <- lapply(1:3, function(i) {
    monty_rng_create(seed = r[, i], n_streams = 1)
  })
  rate <- c(0.1, 0.2, 0.3)
  expect_equal(monty_random_exponential_rate(rate, s),
               mapply(monty_random_exponential_rate, rate, cmp))

  ## Multiple samples case takes a little more effort:
  a <- monty_random_n_exponential_rate(10, rate, s)
  b <- mapply(monty_random_n_exponential_rate, rate, cmp,
              MoreArgs = list(n_samples = 10))
  expect_equal(a, b)
})


test_that("protect against pointer serialisation", {
  s <- monty_rng_create(seed = 42)
  s2 <- unserialize(serialize(s, NULL))
  expect_error(
    monty_random_real(s2),
    "Pointer has been serialised, cannot continue safely (random_real)",
    fixed = TRUE)
})


test_that("validate input size with single stream", {
  s <- monty_rng_create(seed = 42)
  expect_error(monty_random_binomial(10, numeric(0), s),
               "Expected 'prob' to have length 1, not 0")
  expect_error(monty_random_binomial(10, c(0.1, 0.2), s),
               "Expected 'prob' to have length 1, not 2")
})


test_that("validate input size with multiple streams", {
  s <- monty_rng_create(seed = 42, n_streams = 3)
  expect_error(monty_random_binomial(10, numeric(0), s),
               "Expected 'prob' to have length 3 or 1, not 0")
  expect_error(monty_random_binomial(10, c(0.1, 0.2), s),
               "Expected 'prob' to have length 3 or 1, not 2")
})
