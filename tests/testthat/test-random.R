## This file will eventually replace test-rng.R, but until it does we
## use the legacy interface as the source of truth.
test_that("can generate a random number", {
  s <- monty_rng_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_real(s), cmp$random_real(1))
})


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


test_that("can generate a random number from multiple streams at once", {
  s <- monty_rng_create(seed = 42, n_streams = 10)
  cmp <- monty_rng$new(seed = 42, n_streams = 10)
  u <- monty_random_real(s)
  expect_equal(u, drop(cmp$random_real(1)))
  expect_null(dim(u))
})


test_that("can sample multiple random numbers from a single stream", {
  s <- monty_rng_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  u <- monty_random_n_real(10, s)
  expect_null(dim(u))
  expect_equal(drop(u), cmp$random_real(10))
})


test_that("can sample multiple random numbers from multiple streams", {
  s <- monty_rng_create(n_streams = 3, seed = 42)
  cmp <- monty_rng$new(n_streams = 3, seed = 42)
  u <- monty_random_n_real(10, s)
  expect_equal(dim(u), c(10, 3))
  expect_equal(u, cmp$random_real(10))
})


test_that("Preserve stream dimension on generation", {
  s <- monty_rng_create(n_streams = 1, seed = 42,
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
  s <- monty_rng_create(seed = 42, n_streams = 10)
  cmp <- monty_rng$new(seed = 42, n_streams = 10)
  r <- monty_rng_state(s)
  expect_type(r, "raw")
  expect_length(r, 320)
  expect_equal(r, cmp$state())

  monty_rng_set_state(rev(r), s)
  cmp$set_state(rev(r))
  expect_equal(monty_rng_state(s), rev(r))
  expect_equal(monty_random_real(s), drop(cmp$random_real(1)))

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


test_that("can sample from binomial distribution", {
  s <- monty_rng_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_binomial(10, 0.3, s),
               cmp$binomial(1, 10, 0.3))
  expect_equal(monty_random_n_binomial(10, 7, 0.3, s),
               cmp$binomial(10, 7, 0.3))
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


test_that("can sample from exponential distribution (rate)", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_exponential_rate(0.5, s),
               cmp$exponential_rate(1, 0.5))
  expect_equal(monty_random_n_exponential_rate(10, 0.5, s),
               cmp$exponential_rate(10, 0.5))
})


test_that("can sample from exponential distribution (mean)", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_exponential_mean(0.5, s),
               cmp$exponential_mean(1, 0.5))
  expect_equal(monty_random_n_exponential_mean(10, 0.5, s),
               cmp$exponential_mean(10, 0.5))
})


test_that("can sample from poisson distribution", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_poisson(0.5, s),
               cmp$poisson(1, 0.5))
  expect_equal(monty_random_n_poisson(10, 0.5, s),
               cmp$poisson(10, 0.5))
})


test_that("can sample from beta distribution", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_beta(2, 3.1, s),
               cmp$beta(1, 2, 3.1))
  expect_equal(monty_random_n_beta(10, 2, 3.1, s),
               cmp$beta(10, 2, 3.1))
})


test_that("can sample from cauchy distribution", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_cauchy(-1.5, 5, s),
               cmp$cauchy(1, -1.5, 5))
  expect_equal(monty_random_n_cauchy(10, -1.5, 5, s),
               cmp$cauchy(10, -1.5, 5))
})


test_that("can sample from gamma distribution (scale)", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_gamma_scale(2, 5, s),
               cmp$gamma_scale(1, 2, 5))
  expect_equal(monty_random_n_gamma_scale(10, 2, 5, s),
               cmp$gamma_scale(10, 2, 5))
})


test_that("can sample from gamma distribution (rate)", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_gamma_rate(2, 5, s),
               cmp$gamma_rate(1, 2, 5))
  expect_equal(monty_random_n_gamma_rate(10, 2, 5, s),
               cmp$gamma_rate(10, 2, 5))
})


test_that("can sample from negative binomial distribution (prob)", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_negative_binomial_prob(200, 0.05, s),
               cmp$negative_binomial_prob(1, 200, 0.05))
  expect_equal(monty_random_n_negative_binomial_prob(10, 200, 0.05, s),
               cmp$negative_binomial_prob(10, 200, 0.05))
})


test_that("can sample from negative binomial distribution (mu)", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_negative_binomial_mu(200, 65.1, s),
               cmp$negative_binomial_mu(1, 200, 65.1))
  expect_equal(monty_random_n_negative_binomial_mu(10, 200, 65.1, s),
               cmp$negative_binomial_mu(10, 200, 65.1))
})


test_that("can sample from normal distribution", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_normal(-1.5, 5, s),
               cmp$normal(1, -1.5, 5))
  expect_equal(monty_random_n_normal(10, -1.5, 5, s),
               cmp$normal(10, -1.5, 5))
})


test_that("can sample from uniform distribution", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_uniform(-1.5, 5, s),
               cmp$uniform(1, -1.5, 5))
  expect_equal(monty_random_n_uniform(10, -1.5, 5, s),
               cmp$uniform(10, -1.5, 5))
})


test_that("can sample from beta binomial distribution (prob)", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_beta_binomial_prob(100, 0.3, 0.125, s),
               cmp$beta_binomial_prob(1, 100, 0.3, 0.125))
  expect_equal(monty_random_n_beta_binomial_prob(10, 100, 0.3, 0.125, s),
               cmp$beta_binomial_prob(10, 100, 0.3, 0.125))
})


test_that("can sample from beta binomial distribution (ab)", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_beta_binomial_ab(100, 1.5, 8.5, s),
               cmp$beta_binomial_ab(1, 100, 1.5, 8.5))
  expect_equal(monty_random_n_beta_binomial_ab(10, 100, 1.5, 8.5, s),
               cmp$beta_binomial_ab(10, 100, 1.5, 8.5))
})


test_that("can sample from hypergeometric distribution", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_hypergeometric(7, 10, 8, s),
               cmp$hypergeometric(1, 7, 10, 8))
  expect_equal(monty_random_n_hypergeometric(10, 7, 10, 8, s),
               cmp$hypergeometric(10, 7, 10, 8))
})


test_that("can sample from truncated normal distribution", {
  s <- monty_random_create(seed = 42)
  cmp <- monty_rng$new(seed = 42)
  expect_equal(monty_random_truncated_normal(1, 4, -2, 3, s),
               cmp$truncated_normal(1, 1, 4, -2, 3))
  expect_equal(monty_random_n_truncated_normal(10, 1, 4, -2, 3, s),
               cmp$truncated_normal(10, 1, 4, -2, 3))
})
