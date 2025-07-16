test_that("can generate random numbers", {
  ans1 <- monty_random_n_real(100, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_real(100, monty_rng_create(seed = 1))
  ans3 <- monty_random_n_real(100, monty_rng_create(seed = 2))
  expect_equal(length(ans1), 100)
  expect_identical(ans1, ans2)
  expect_false(any(ans1 == ans3))
})


test_that("Create interleaved rng", {
  n <- 128
  seed <- 1

  rng1 <- monty_rng_create(seed = seed, n_streams = 1L)
  rng2 <- monty_rng_create(seed = seed, n_streams = 2L)
  rng3 <- monty_rng_create(seed = seed, n_streams = 4L)
  rng4 <- monty_rng_create(seed = seed, n_streams = 8L)

  ans1 <- monty_random_n_real(n, rng1)
  ans2 <- monty_random_n_real(n, rng2)
  ans3 <- monty_random_n_real(n, rng3)
  ans4 <- monty_random_n_real(n, rng4)

  ## We can find elements from the each rng through the larger
  ## sequences:
  expect_identical(ans1, ans2[, 1])
  expect_identical(ans1, ans3[, 1])
  expect_identical(ans1, ans3[, 1])
  expect_identical(ans2, ans3[, 1:2])
  expect_identical(ans2, ans4[, 1:2])
  expect_identical(ans3, ans4[, 1:4])

  expect_equal(length(rng1), 1)
  expect_equal(length(rng2), 2)
  expect_equal(length(rng3), 4)
  expect_equal(length(rng4), 8)
})


test_that("run uniform random numbers", {
  ans1 <- monty_random_n_real(100, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_real(100, monty_rng_create(seed = 1))
  ans3 <- monty_random_n_uniform(100, 0, 1, monty_rng_create(seed = 1))
  ans4 <- monty_random_n_uniform(100, 0, 1, monty_rng_create(seed = 2))

  expect_true(all(ans1 >= 0))
  expect_true(all(ans1 <= 1))
  expect_identical(ans1, ans2)
  expect_identical(ans1, ans3)
  expect_false(any(ans1 == ans4))
})


test_that("run uniform random numbers with odd bounds", {
  ans <- monty_random_n_uniform(100, -100, 100, monty_rng_create(seed = 1))
  expect_true(any(ans > 0))
  expect_true(any(ans < 0))
  expect_true(all(ans >= -100))
  expect_true(all(ans <= 100))
})


test_that("distribution of uniform numbers", {
  m <- 100000
  a <- exp(1)
  b <- pi
  ans <- monty_random_n_uniform(m, a, b, monty_rng_create(seed = 1))
  expect_equal(mean(ans), (a + b) / 2, tolerance = 1e-3)
  expect_equal(var(ans), (b - a)^2 / 12, tolerance = 1e-2)
})


test_that("run binomial random numbers", {
  m <- 100000
  n <- 100
  p <- 0.1

  ans1 <- monty_random_n_binomial(m, n, p, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_binomial(m, n, p, monty_rng_create(seed = 1))
  expect_identical(ans1, ans2)

  ## Should do this with much more statistical rigour, but this looks
  ## pretty good.
  expect_equal(mean(ans1), n * p, tolerance = 1e-3)
  expect_equal(var(ans1), n * p * (1 - p), tolerance = 1e-2)
})


test_that("binomial numbers run the short circuit path", {
  m <- 10000
  n <- 100
  p <- 0.1

  expect_identical(monty_random_n_binomial(m, 0, p, monty_rng_create(seed = 1)),
                   rep(0, m))
  expect_identical(monty_random_n_binomial(m, n, 0, monty_rng_create(seed = 1)),
                   rep(0, m))
  expect_identical(monty_random_n_binomial(m, n, 1, monty_rng_create(seed = 1)),
                   rep(as.numeric(n), m))
})


test_that("binomial numbers on the 'small' path", {
  m <- 500000
  n <- 20
  p <- 0.2

  ans1 <- monty_random_n_binomial(m, n, p, monty_rng_create(seed = 1))
  expect_equal(mean(ans1), n * p, tolerance = 1e-3)
  expect_equal(var(ans1), n * p * (1 - p), tolerance = 1e-2)
})


test_that("binomial numbers and their complement are the same (np small)", {
  m <- 100
  n <- 20
  p <- 0.2

  ans1 <- monty_random_n_binomial(m, n, p, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_binomial(m, n, 1 - p, monty_rng_create(seed = 1))
  expect_equal(ans1, n - ans2)
})


test_that("binomial numbers and their complement are the same (np large)", {
  m <- 100
  n <- 200
  p <- 0.2

  ans1 <- monty_random_n_binomial(m, n, p, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_binomial(m, n, 1 - p, monty_rng_create(seed = 1))
  expect_equal(ans1, n - ans2)
})


test_that("Binomial random numbers prevent bad inputs", {
  skip_on_cran() # potentially system dependent
  r <- monty_rng_create(seed = 1)
  monty_random_binomial(0, 0, r)
  expect_error(
    monty_random_binomial(1, -1, r),
    "Invalid call to binomial with n = 1, p = -1")
  expect_error(
    monty_random_binomial(1, 0 - 1e-8, r),
    "Invalid call to binomial with n = 1, p = -1e-08")
  expect_error(
    monty_random_binomial(1, 2, r),
    "Invalid call to binomial with n = 1, p = 2")
  ## TODO: this is not a great error here, but there's not much that
  ## can be done without a lot of faff with the underlying print
  expect_error(
    monty_random_binomial(1, 1 + 1e-8, r),
    "Invalid call to binomial with n = 1, p = 1")
  expect_error(
    monty_random_binomial(-1, 0.5, r),
    "Invalid call to binomial with n = -1, p = 0.5")
  expect_error(
    monty_random_binomial(1, NaN, r),
    "Invalid call to binomial with n = 1, p = .+")
  expect_error(
    monty_random_binomial(NaN, 1, r),
    "Invalid call to binomial with n = .+, p = 1")
})


test_that("avoid integer overflow in binomial draws with very large n", {
  r <- monty_rng_create(seed = 1)
  n <- 2^33
  expect_equal(monty_random_binomial(n, 0, r), 0)
  expect_equal(monty_random_binomial(n, 1, r), n)

  p1 <- 2.5e-10
  m1 <- monty_random_n_binomial(1000000, n, p1, r)
  expect_equal(mean(m1), n * p1, tolerance = 1e-3)
  mt1 <- tabulate(m1)
  expect_false(any(mt1) == 0)

  p2 <- 0.1
  m2 <- monty_random_n_binomial(1000000, n, p2, r)
  mt2 <- tabulate(m2)
  expect_equal(mean(m2), n * p2, tolerance = 1e-3)
  expect_false(any(mt1) == 0)
})


test_that("poisson numbers", {
  n <- 100000
  lambda <- 5

  ans1 <- monty_random_n_poisson(n, lambda, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_poisson(n, lambda, monty_rng_create(seed = 1))
  ans3 <- monty_random_n_poisson(n, lambda, monty_rng_create(seed = 2))
  expect_identical(ans1, ans2)
  expect_false(all(ans1 == ans3))

  expect_equal(mean(ans1), lambda, tolerance = 1e-2)
  expect_equal(var(ans1), lambda, tolerance = 1e-2)
})


test_that("Big poisson numbers", {
  n <- 100000
  lambda <- 20

  ans1 <- monty_random_n_poisson(n, lambda, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_poisson(n, lambda, monty_rng_create(seed = 1))
  ans3 <- monty_random_n_poisson(n, lambda, monty_rng_create(seed = 2))
  expect_identical(ans1, ans2)
  expect_false(all(ans1 == ans3))

  expect_equal(mean(ans1), lambda, tolerance = 1e-2)
  expect_equal(var(ans1), lambda, tolerance = 1e-2)
})


test_that("big poisson numbers at edge of transition are ok", {
  n <- 100000
  lambda <- 1e8 - 1
  rng <- monty_rng_create(seed = 1)
  ans <- monty_random_n_poisson(n, lambda, rng)
  expect_equal(mean(ans), lambda, tolerance = 1e-2)
  expect_equal(var(ans), lambda, tolerance = 1e-2)
})


test_that("Very big poisson numbers", {
  n <- 100000
  lambda <- 1e12

  ans1 <- monty_random_n_poisson(n, lambda, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_poisson(n, lambda, monty_rng_create(seed = 1))
  ans3 <- monty_random_n_poisson(n, lambda, monty_rng_create(seed = 2))
  expect_identical(ans1, ans2)
  expect_false(all(ans1 == ans3))

  expect_equal(mean(ans1), lambda, tolerance = 1e-2)
  expect_equal(var(ans1), lambda, tolerance = 1e-2)
})


test_that("Poisson numbers only valid for 0 <= lambda < Inf", {
  n <- 100
  r <- monty_rng_create()
  expect_error(monty_random_poisson(-1, r),
               "Invalid call to Poisson")
  expect_error(monty_random_poisson(Inf, r),
               "Invalid call to Poisson")
})


test_that("Short circuit exit does not update rng state", {
  rng <- monty_rng_create(seed = 1)
  s <- monty_rng_state(rng)
  ans <- monty_random_n_poisson(100, 0, rng)
  expect_equal(ans, rep(0, 100))
  expect_identical(monty_rng_state(rng), s)
})


test_that("normal (box_muller) agrees with stats::rnorm", {
  n <- 100000
  ans <- monty_random_n_normal(n, 0, 1, monty_rng_create(seed = 2),
                               "box_muller")
  expect_equal(mean(ans), 0, tolerance = 1e-2)
  expect_equal(sd(ans), 1, tolerance = 1e-2)
  expect_gt(ks.test(ans, "pnorm")$p.value, 0.1)
})


test_that("normal (polar) agrees with stats::rnorm", {
  n <- 100000
  ans <- monty_random_n_normal(n, 0, 1, monty_rng_create(seed = 2), "polar")
  expect_equal(mean(ans), 0, tolerance = 1e-2)
  expect_equal(sd(ans), 1, tolerance = 1e-2)
  expect_gt(ks.test(ans, "pnorm")$p.value, 0.1)
})


test_that("normal (ziggurat) agrees with stats::rnorm", {
  n <- 100000
  ans <- monty_random_n_normal(n, 0, 1, monty_rng_create(seed = 2), "ziggurat")
  expect_equal(mean(ans), 0, tolerance = 1e-2)
  expect_equal(sd(ans), 1, tolerance = 1e-2)
  expect_gt(ks.test(ans, "pnorm")$p.value, 0.1)
})


test_that("can draw normals with different algorithms", {
  r1 <- monty_rng_create(seed = 1)
  r2 <- monty_rng_create(seed = 1)

  expect_equal(replicate(3, monty_random_normal(0, 1, r1, "box_muller")),
               monty_random_n_normal(3, 0, 1, r2, "box_muller"))
  expect_equal(replicate(3, monty_random_normal(0, 1, r1, "polar")),
               monty_random_n_normal(3, 0, 1, r2, "polar"))
  expect_equal(replicate(3, monty_random_normal(0, 1, r1, "ziggurat")),
               monty_random_n_normal(3, 0, 1, r2, "ziggurat"))
})


test_that("normal scales draws", {
  n <- 100
  mean <- exp(1)
  sd <- pi
  rng1 <- monty_rng_create(seed = 1)
  rng2 <- monty_rng_create(seed = 1)
  expect_equal(monty_random_n_normal(n, mean, sd, rng1, "box_muller"),
               mean + sd * monty_random_n_normal(n, 0, 1, rng2, "box_muller"))
  expect_equal(monty_random_n_normal(n, mean, sd, rng1, "polar"),
               mean + sd * monty_random_n_normal(n, 0, 1, rng2, "polar"))
  expect_equal(monty_random_n_normal(n, mean, sd, rng1, "ziggurat"),
               mean + sd * monty_random_n_normal(n, 0, 1, rng2, "ziggurat"))
})


test_that("Prevent unknown normal algorithms", {
  expect_error(
    monty_random_normal(0, 1, monty_rng_create(seed = 2), "monty_python"),
    "Unknown normal algorithm 'monty_python'")
  expect_error(
    monty_random_n_normal(5, 0, 1, monty_rng_create(seed = 2), "monty_python"),
    "Unknown normal algorithm 'monty_python'")
})


test_that("rexp agrees with stats::rexp", {
  n <- 100000
  rate <- 0.04
  ans <- monty_random_n_exponential_rate(n, rate, monty_rng_create(seed = 2))
  expect_equal(mean(ans), 1 / rate, tolerance = 1e-2)
  expect_equal(var(ans), 1 / rate^2, tolerance = 1e-2)
  expect_gt(ks.test(ans, "pexp", rate)$p.value, 0.1)
})



test_that("can sample from exponential distribution (mean)", {
  r1 <- monty_rng_create(seed = 42)
  r2 <- monty_rng_create(seed = 42)
  expect_equal(monty_random_exponential_mean(0.5, r1),
               monty_random_exponential_rate(2, r2))
  expect_equal(monty_random_n_exponential_mean(10, 0.5, r1),
               monty_random_n_exponential_rate(10, 2, r2))
})



test_that("continue stream", {
  rng1 <- monty_rng_create(seed = 1)
  rng2 <- monty_rng_create(seed = 1)

  y1 <- monty_random_n_uniform(100, 0, 1, rng1)
  y2_1 <- monty_random_n_uniform(50, 0, 1, rng2)
  y2_2 <- monty_random_n_uniform(50, 0, 1, rng2)
  y2 <- c(y2_1, y2_2)
  expect_identical(y1, y2)
})


test_that("jump", {
  seed <- 1
  rng1a <- monty_rng_create(seed = seed)
  rng1b <- monty_rng_jump(monty_rng_create(seed = seed))
  rng2 <- monty_rng_create(seed = seed, n_streams = 2L)

  r2 <- monty_random_n_real(10, rng2)
  r1a <- monty_random_n_real(10, rng1a)
  r1b <- monty_random_n_real(10, rng1b)

  expect_equal(cbind(r1a, r1b, deparse.level = 0), r2)
})


test_that("long jump", {
  seed <- 1
  rng1 <- monty_rng_create(seed = seed)
  rng2 <- monty_rng_jump(monty_rng_create(seed = seed))
  rng3 <- monty_rng_long_jump(monty_rng_create(seed = seed))
  rng4 <- monty_rng_jump(monty_rng_long_jump(monty_rng_create(seed = seed)))

  r1 <- monty_random_n_real(20, rng1)
  r2 <- monty_random_n_real(20, rng2)
  r3 <- monty_random_n_real(20, rng3)
  r4 <- monty_random_n_real(20, rng4)

  expect_true(all(r1 != r2))
  expect_true(all(r1 != r3))
  expect_true(all(r1 != r4))
  expect_true(all(r2 != r3))
  expect_true(all(r2 != r4))
  expect_true(all(r3 != r4))
})


test_that("get state", {
  seed <- 1
  rng1 <- monty_rng_create(seed = seed)
  rng2 <- monty_rng_create(seed = seed)
  rng3 <- monty_rng_create(seed = seed, n_streams = 2)

  s1 <- monty_rng_state(rng1)
  expect_type(s1, "raw")
  expect_equal(length(s1), 32)

  s2 <- monty_rng_state(rng2)
  expect_identical(s2, s1)

  s3 <- monty_rng_state(rng3)
  expect_equal(length(s3), 64)
  expect_identical(s3[seq_len(32)], s1)
  expect_identical(s3[-seq_len(32)], monty_rng_state(monty_rng_jump(rng2)))
})


test_that("initialise single rng with binary state", {
  seed <- 42
  rng1 <- monty_rng_create(seed = seed)
  state <- monty_rng_state(rng1)
  rng2 <- monty_rng_create(seed = state)
  expect_identical(monty_rng_state(rng1), monty_rng_state(rng2))
  r1 <- monty_random_n_real(10, rng1)
  r2 <- monty_random_n_real(10, rng2)
  expect_identical(r1, r2)
  expect_identical(monty_rng_state(rng1), monty_rng_state(rng2))
})


test_that("initialise parallel rng with binary state", {
  seed <- 42
  rng1 <- monty_rng_create(seed = seed, n_streams = 5)
  state <- monty_rng_state(rng1)
  rng2 <- monty_rng_create(seed = state, n_streams = 5)
  r1 <- monty_random_n_real(10, rng1)
  r2 <- monty_random_n_real(10, rng2)
  expect_identical(r1, r2)
  expect_identical(monty_rng_state(rng1), monty_rng_state(rng2))
})


test_that("initialise parallel rng with single binary state and jump", {
  seed <- 42
  rng1 <- monty_rng_create(seed = seed, n_streams = 1)
  rng2 <- monty_rng_create(seed = seed, n_streams = 2)
  state <- monty_rng_state(rng1)
  rng3 <- monty_rng_create(seed = state, n_streams = 2)
  expect_identical(monty_rng_state(rng3), monty_rng_state(rng2))
})


test_that("initialise parallel rng with binary state and drop", {
  seed <- 42
  rng10 <- monty_rng_create(seed = seed, n_streams = 10)
  rng5 <- monty_rng_create(seed = seed, n_streams = 5)
  len <- 5 * 32
  expect_identical(monty_rng_state(rng5), monty_rng_state(rng10)[seq_len(len)])
})


test_that("require that raw vector is of sensible size", {
  expect_error(monty_rng_create(seed = raw()),
               "Expected raw vector of length as multiple of 32 for 'seed'")
  expect_error(monty_rng_create(seed = raw(31)),
               "Expected raw vector of length as multiple of 32 for 'seed'")
  expect_error(monty_rng_create(seed = raw(63)),
               "Expected raw vector of length as multiple of 32 for 'seed'")
})


test_that("initialise with NULL, generating a seed from R", {
  set.seed(1)
  rng1 <- monty_rng_create(seed = NULL)
  set.seed(1)
  rng2 <- monty_rng_create(seed = NULL)
  rng3 <- monty_rng_create(seed = NULL)
  set.seed(1)

  expect_identical(monty_rng_state(rng2), monty_rng_state(rng1))
  expect_false(identical(monty_rng_state(rng3), monty_rng_state(rng2)))
})


test_that("can't create rng with silly things", {
  expect_error(
    monty_rng_create(seed = mtcars),
    "Invalid type for 'seed'")
  expect_error(
    monty_rng_create(seed = function(x) 2),
    "Invalid type for 'seed'")
})


test_that("negative seed values result in sensible state", {
  ## Don't end up with all-zero state, and treat different negative
  ## numbers as different (don't truncate to zero or anything
  ## pathalogical)
  s0 <- monty_rng_state(monty_rng_create(seed = 0))
  s1 <- monty_rng_state(monty_rng_create(seed = -1))
  s10 <- monty_rng_state(monty_rng_create(seed = -10))

  expect_false(all(s0 == as.raw(0)))
  expect_false(all(s1 == as.raw(0)))
  expect_false(all(s10 == as.raw(0)))
  skip_on_os("mac") # mrc-5288
  expect_false(identical(s0, s1))
  expect_false(identical(s0, s10))
  expect_false(identical(s1, s10))
})


test_that("multinomial algorithm is correct", {
  set.seed(1)
  prob <- runif(10)
  prob <- prob / sum(prob)
  size <- 20
  n <- 5

  ## Separate implementation of the core algorithm:
  cmp_multinomial <- function(rng, size, prob) {
    p <- prob / (1 - cumsum(c(0, prob[-length(prob)])))
    ret <- numeric(length(prob))
    for (i in seq_len(length(prob) - 1L)) {
      ret[i] <- monty_random_binomial(size, p[i], rng)
      size <- size - ret[i]
    }
    ret[length(ret)] <- size
    ret
  }

  r1 <- monty_rng_create(seed = 1)
  r2 <- monty_rng_create(seed = 1)
  res1 <- monty_random_multinomial(size, prob, r1)
  res2 <- cmp_multinomial(r2, size, prob)
  expect_equal(res1, res2)

  res1 <- monty_random_n_multinomial(n, size, prob, r1)
  res2 <- replicate(n, cmp_multinomial(r2, size, prob))
  expect_equal(res1, res2)
})


test_that("multinomial expectation is correct", {
  p <- runif(10)
  p <- p / sum(p)
  n <- 10000
  rng <- monty_rng_create(seed = 1)
  res <- monty_random_n_multinomial(n, 100, p, rng)
  expect_equal(dim(res), c(10, n))
  expect_equal(colSums(res), rep(100, n))
  expect_equal(rowMeans(res), p * 100, tolerance = 1e-2)
})


test_that("multinomial allows zero probs", {
  p <- runif(10)
  p[4] <- 0
  p <- p / sum(p)
  n <- 500
  size <- 100
  rng <- monty_rng_create(seed = 1)
  res <- monty_random_n_multinomial(n, size, p, rng)

  expect_equal(res[4, ], rep(0, n))
  expect_equal(colSums(res), rep(size, n))
})


test_that("multinomial allows non-normalised prob", {
  p <- runif(10, 0, 10)
  n <- 50
  res1 <- monty_random_n_multinomial(n, 100, p, monty_rng_create(seed = 1))
  res2 <- monty_random_n_multinomial(n, 100, p / sum(p),
                                     monty_rng_create(seed = 1))
  expect_equal(res1, res2)
})


test_that("Invalid prob throws an error", {
  r <- monty_rng_create(seed = 1)
  expect_error(
    monty_random_multinomial(10, c(0, 0, 0), r),
    "No positive prob in call to multinomial")
  expect_error(
    monty_random_multinomial(10, c(-0.1, 0.6, 0.5), r),
    "Negative prob passed to multinomial")
})


test_that("Can vary scalar parameters by generator for multinomial", {
  np <- 7L
  ng <- 3L
  n <- 17L

  prob <- runif(np)
  size <- 10 * seq_len(ng)

  r <- monty_rng_create(seed = 1, n_streams = ng)
  state <- matrix(monty_rng_state(r), ncol = ng)

  cmp <- vapply(seq_len(ng), function(i) {
    ri <- monty_rng_create(seed = state[, i])
    monty_random_n_multinomial(n, size[i], prob, ri)
  }, matrix(numeric(), np, n))

  res <- monty_random_n_multinomial(n, size, prob, r)
  expect_equal(res, cmp)
})


test_that("Can vary vector parameters by generator for multinomial", {
  np <- 7L
  ng <- 3L
  size <- 13
  n <- 17L

  prob <- matrix(runif(np * ng), np, ng)

  r <- monty_rng_create(seed = 1, n_streams = ng)
  state <- matrix(monty_rng_state(r), ncol = ng)

  cmp <- vapply(seq_len(ng), function(i) {
    ri <- monty_rng_create(seed = state[, i])
    monty_random_n_multinomial(n, size, prob[, i], ri)
  }, matrix(numeric(), np, n))

  res <- monty_random_n_multinomial(n, size, prob, r)
  expect_equal(res, cmp)
})


test_that("can validate multinomial inputs", {
  r <- monty_rng_create(seed = 1, n_streams = 3)
  expect_error(
    monty_random_multinomial(5, matrix(1, 7, 2), r),
    "Expected 'prob' to have 3 or 1 columns, not 2")
  expect_error(
    monty_random_multinomial(5, array(1, c(7, 2, 3)), r),
    "Expected 'prob' to be a matrix")

  r <- monty_rng_create(seed = 1, n_streams = 1)
  expect_error(
    monty_random_multinomial(5, matrix(1, 7, 2), r),
    "Expected 'prob' to have 1 column, not 2")
})


test_that("deterministic multinomial returns expectation", {
  r <- monty_rng_create(seed = 1, deterministic = TRUE)
  p <- runif(10)
  p <- p / sum(p)
  res <- monty_random_multinomial(100, p, r)
  expect_equal(res, 100 * p)
})


test_that("deterministic rbinom returns mean", {
  m <- 10
  n <- as.numeric(sample(10, m, replace = TRUE))
  p <- runif(m)

  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)

  expect_equal(
    mapply(monty_random_binomial, n, p, MoreArgs = list(state = rng)),
    n * p)

  expect_equal(monty_rng_state(rng), state)
})


test_that("deterministic rbinom accepts non-integer size", {
  m <- 10
  n <- runif(m, 0, 10)
  p <- runif(m)
  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)
  expect_equal(
    mapply(monty_random_binomial, n, p, MoreArgs = list(state = rng)),
    n * p)
  expect_equal(monty_rng_state(rng), state)
})


test_that("deterministic rbinom allow small negative innacuracies", {
  rng <- monty_rng_create(seed = 1, deterministic = TRUE)

  eps <- .Machine$double.eps

  expect_identical(monty_random_binomial(0, 0.5, rng), 0.0)
  expect_identical(monty_random_binomial(-eps, 0.5, rng), 0.0)

  expect_error(monty_random_binomial(-sqrt(eps * 1.1), 0.5, rng),
               "Invalid call to binomial with n = -")
})


test_that("deterministic rpois returns mean", {
  m <- 10
  ## numbers from all three regimes:
  lambda <- c(
    runif(m, 0, 10),
    runif(m, 10, 1000),
    runif(m, 1e10, 1e12))
  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)
  expect_equal(vnapply(lambda, monty_random_poisson, rng), lambda)
  expect_equal(monty_rng_state(rng), state)
})


test_that("deterministic runif returns mean", {
  m <- 10
  l <- runif(m, -10, 10)
  u <- l + runif(m, 0, 10)
  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)
  expect_equal(
    mapply(monty_random_uniform, l, u, MoreArgs = list(state = rng)),
    (l + u) / 2)
  expect_equal(monty_rng_state(rng), state)
})


test_that("deterministic rexp returns mean", {
  m <- 10
  rate <- runif(m, 0, 10)
  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)
  expect_equal(vnapply(rate, monty_random_exponential_rate, rng), 1 / rate)
  expect_equal(monty_rng_state(rng), state)
})


test_that("deterministic rnorm returns mean", {
  m <- 10
  mu <- runif(m, -10, 10)
  sd <- runif(m, 0, 10)
  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)
  expect_equal(
    mapply(monty_random_normal, mu, sd, MoreArgs = list(state = rng)),
    mu)
  expect_equal(monty_rng_state(rng), state)
})


test_that("We can load the example rng package", {
  skip_for_compilation()
  skip_on_os("windows")

  path_src <- monty_file("random/package")
  tmp <- tempfile()
  copy_directory(path_src, tmp)
  cpp11::cpp_register(tmp, quiet = TRUE)

  pkg <- pkgload::load_all(tmp, export_all = FALSE, quiet = TRUE)
  ans <- pkg$env$random_normal(10, 0, 1, 42)
  cmp <- monty_random_n_normal(10, 0, 1, monty_rng_create(seed = 42))
  expect_equal(ans, cmp)

  pkgload::unload("rnguse")
  unlink(tmp, recursive = TRUE)
})


test_that("We can compile the standalone program", {
  skip_for_compilation()
  skip_on_os("windows")

  path_src <- monty_file("random/openmp")
  tmp <- tempfile()
  copy_directory(path_src, tmp)

  args <- c(dirname(monty_file("include")), "--no-openmp")

  code <- with_dir(
    tmp,
    system2("./configure", args, stdout = FALSE, stderr = FALSE))
  expect_equal(code, 0)
  code <- with_dir(
    tmp,
    system2("make", stdout = FALSE, stderr = FALSE))
  expect_equal(code, 0)

  res <- system2(file.path(tmp, "rnguse"), c("10", "5", "42"),
                 stdout = TRUE)
  ans <- as.numeric(sub("[0-9]: ", "", res))

  r <- monty_rng_create(seed = 42, n_streams = 5)
  cmp <- colSums(monty_random_n_uniform(10, 0, 1, r))
  expect_equal(ans, cmp)
})


## Testing for the hypergeometric is a little different to above as it
## turns out that despite the very different underlying
## implementations, the algorithm in the paper
## (https://www.tandfonline.com/doi/abs/10.1080/00949658508810839) has
## been followed pretty exactly. So if we were willing to assume that
## the R version is correct we can just check that the stream of
## numbers is exactly the same.
##
## We do this twice - once between R's rhyper and a direct
## reimplementation of the Rust code in R (within helper-rng) using
## R's RNG stream, then again between our R reimplementation and
## monty's idiomatic C++ version.
test_that("hypergeometric reference implementation agrees with R", {
  testthat::skip_on_cran() # subject to change beyond our control
  hypergeometric <- hypergeometric_r(function() runif(1))

  ## There are three branches to consider:
  ## Case 1, use HIP (inversion) algorithm
  m <- 7
  n <- 10
  k <- 8

  set.seed(1)
  r1 <- rhyper(500, m, n, k)
  set.seed(1)
  r2 <- replicate(500, hypergeometric(m, n, k))
  expect_equal(r1, r2)

  ## Case 1a, HIP but sample exactly half
  m <- 5
  n <- 5
  k <- 5
  set.seed(1)
  r1 <- rhyper(5000, m, n, k)
  set.seed(1)
  r2 <- replicate(5000, hypergeometric(m, n, k))
  ## Here, we differ only because of the sign but as black and white
  ## are exchangeable here this is equivalent.
  expect_equal(r1, 5 - r2)

  ## Case 2, use H2PE algorithm, simple exit
  m <- 70
  n <- 100
  k <- 80
  set.seed(1)
  r1 <- rhyper(500, m, n, k)
  set.seed(1)
  r2 <- replicate(500, hypergeometric(m, n, k))
  expect_equal(r1, r2)

  ## Case 3, use H2PE algorithm, squeezing exit
  m <- 700
  n <- 1000
  k <- 800
  set.seed(1)
  r1 <- rhyper(500, m, n, k)
  set.seed(1)
  r2 <- replicate(500, hypergeometric(m, n, k))
  expect_equal(r1, r2)
})


## Found by Charlie, this overflowed and generated garbage answers
## below 0.14.x
test_that("avoid integer overflow", {
  r1 <- monty_rng_create(seed = 1)
  r2 <- monty_rng_create(seed = 1)
  hypergeometric <- hypergeometric_r(function() monty_random_real(r1))
  n1 <- 4334282
  n2 <- 5665718
  size <- 750
  ans1 <- replicate(100, hypergeometric(n1, n2, size))
  ans2 <- monty_random_n_hypergeometric(100, n1, n2, size, r2)
  expect_equal(ans1, ans2)
})


test_that("rng agrees with hypergeometric reference implementation", {
  rng1 <- monty_rng_create(seed = 1L)
  rng2 <- monty_rng_create(seed = 1L)
  hypergeometric <- hypergeometric_r(function() monty_random_real(rng1))

  ## Same three cases as above:
  ## Case 1, use HIP (inversion) algorithm
  m <- 7
  n <- 10
  k <- 8
  r1 <- replicate(500, hypergeometric(m, n, k))
  r2 <- monty_random_n_hypergeometric(500, m, n, k, rng2)
  expect_equal(r1, r2)

  ## Case 1a, HIP but sample exactly half
  m <- 5
  n <- 5
  k <- 5
  r1 <- replicate(500, hypergeometric(m, n, k))
  r2 <- monty_random_n_hypergeometric(500, m, n, k, rng2)
  expect_equal(r1, r2)

  ## Case 2, use H2PE algorithm, simple exit
  m <- 70
  n <- 100
  k <- 80
  r1 <- replicate(500, hypergeometric(m, n, k))
  r2 <- monty_random_n_hypergeometric(500, m, n, k, rng2)
  expect_equal(r1, r2)

  ## Case 3, use H2PE algorithm, squeezing exit
  m <- 700
  n <- 1000
  k <- 800
  r1 <- replicate(500, hypergeometric(m, n, k))
  r2 <- monty_random_n_hypergeometric(500, m, n, k, rng2)
  expect_equal(r1, r2)
})


test_that("symmetry property around n1/n2 and k holds", {
  n1 <- 7
  n2 <- 15
  k <- 5
  n <- n1 + n2
  rng <- function() {
    monty_rng_create(seed = 11)
  }

  r1 <- monty_random_n_hypergeometric(500, n1, n2, k, rng())
  r2 <- monty_random_n_hypergeometric(500, n2, n1, k, rng())
  r3 <- monty_random_n_hypergeometric(500, n1, n2, n - k, rng())
  r4 <- monty_random_n_hypergeometric(500, n2, n1, n - k, rng())

  expect_equal(r2, k - r1)
  expect_equal(r3, n1 - r1)
  expect_equal(r4, r1 + n2 - k)
})


test_that("deterministic hypergeometric returns mean", {
  n_reps <- 10
  n1 <- as.numeric(sample(10, n_reps, replace = TRUE))
  n2 <- as.numeric(sample(10, n_reps, replace = TRUE))
  n <- n1 + n2
  k <- floor(runif(n_reps, 0, n))

  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)

  expect_equal(
    mapply(monty_random_hypergeometric, n1, n2, k, MoreArgs = list(rng)),
    k * n1 / n)

  expect_equal(monty_rng_state(rng), state)
})


test_that("hypergeometric random numbers prevent bad inputs", {
  r <- monty_rng_create(seed = 1)
  expect_equal(monty_random_hypergeometric(0, 0, 0, r), 0)

  expect_error(
    monty_random_hypergeometric(-1, 5, 2, r),
    "Invalid call to hypergeometric with n1 = -1, n2 = 5, k = 2")
  expect_error(
    monty_random_hypergeometric(5, -1, 2, r),
    "Invalid call to hypergeometric with n1 = 5, n2 = -1, k = 2")
  expect_error(
    monty_random_hypergeometric(5, 3, -2, r),
    "Invalid call to hypergeometric with n1 = 5, n2 = 3, k = -2")
  expect_error(
    monty_random_hypergeometric(5, 3, 10, r),
    "Invalid call to hypergeometric with n1 = 5, n2 = 3, k = 10")
})


test_that("fast exits do not draw random numbers", {
  r <- monty_rng_create(seed = 1)
  s <- monty_rng_state(r)

  ## If there's nothing sampled from nothing, return nothing
  expect_equal(monty_random_hypergeometric(0, 0, 0, r), 0)
  ## If there's nothing sampled from something, return nothing
  expect_equal(monty_random_hypergeometric(10, 5, 0, r), 0)
  ## If there's nothing to choose from, take the only option
  expect_equal(monty_random_hypergeometric(10, 0, 2, r), 2)
  expect_equal(monty_random_hypergeometric(0, 10, 2, r), 0)
  ## If we select everything, return everything
  expect_equal(monty_random_hypergeometric(10, 5, 15, r), 10)
  expect_equal(monty_random_hypergeometric(5, 10, 15, r), 5)

  expect_identical(monty_rng_state(r), s)
})


test_that("gamma for a = 1 is the same as exponential", {
  rng1 <- monty_rng_create(seed = 1L)
  rng2 <- monty_rng_create(seed = 1L)

  n <- 10
  b <- 3

  gamma <- monty_random_n_gamma_scale(n, 1, b, rng1)
  exp <- monty_random_n_exponential_rate(n, 1 / b, rng2)

  expect_equal(gamma, exp)
})


test_that("gamma_rate follows from gamma_scale", {
  rng1 <- monty_rng_create(seed = 1L)
  rng2 <- monty_rng_create(seed = 1L)
  b <- 3
  expect_identical(
    monty_random_n_gamma_rate(100, 5, 1 / b, rng1),
    monty_random_n_gamma_scale(100, 5, b, rng2))
})


test_that("can draw gamma random numbers", {
  ## when a > 1
  a <- 5
  b <- 3
  n <- 10000000

  ans1 <- monty_random_n_gamma_scale(n, a, b, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_gamma_scale(n, a, b, monty_rng_create(seed = 1))
  expect_identical(ans1, ans2)

  expect_equal(mean(ans1), a * b, tolerance = 1e-3)
  expect_equal(var(ans1), a * b^2, tolerance = 1e-3)

  ## when a < 1
  a <- 0.5

  ans3 <- monty_random_n_gamma_scale(n, a, b, monty_rng_create(seed = 1))
  ans4 <- monty_random_n_gamma_scale(n, a, b, monty_rng_create(seed = 1))
  expect_identical(ans3, ans4)

  expect_equal(mean(ans3), a * b, tolerance = 1e-3)
  expect_equal(var(ans3), a * b^2, tolerance = 1e-3)
})


test_that("deterministic gamma returns mean", {
  n_reps <- 10
  a <- as.numeric(sample(10, n_reps, replace = TRUE))
  b <- as.numeric(sample(10, n_reps, replace = TRUE))

  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)

  expect_equal(
    mapply(monty_random_gamma_scale, a, b, MoreArgs = list(rng)),
    a * b)
  expect_equal(monty_rng_state(rng), state)
})


test_that("gamma random numbers prevent bad inputs", {
  r <- monty_rng_create(seed = 1)
  expect_equal(monty_random_gamma_scale(0, 0, r), 0)
  expect_equal(monty_random_gamma_scale(Inf, Inf, r), Inf)

  expect_error(
    monty_random_gamma_scale(-1.1, 5.1, r),
    "Invalid call to gamma with shape = -1.1, scale = 5.1")
  expect_error(
    monty_random_gamma_scale(5.1, -1.1, r),
    "Invalid call to gamma with shape = 5.1, scale = -1.1")
})


test_that("can generate negative binomial numbers", {
  m <- 1000000
  n <- 958
  p <- 0.004145
  r <- monty_rng_create(seed = 1)
  yf <- monty_random_n_negative_binomial_prob(m, n, p, r)

  expect_equal(mean(yf), (1 - p) * n / p, tolerance = 1e-3)
  expect_equal(var(yf), ((1 - p) * n) / p^2, tolerance = 1e-2)
})


test_that("negative_binomial_mu follows from negative_binomial_prob", {
  rng1 <- monty_rng_create(seed = 1L)
  rng2 <- monty_rng_create(seed = 1L)
  prob <- 0.3
  size <- 20
  mu <- size * (1 - prob) / prob
  expect_identical(
    monty_random_negative_binomial_mu(size, mu, rng1),
    monty_random_negative_binomial_prob(size, prob, rng2))
  expect_identical(
    monty_random_n_negative_binomial_mu(100, size, mu, rng1),
    monty_random_n_negative_binomial_prob(100, size, prob, rng2))
})


test_that("deterministic negative binomial returns mean", {
  m <- 100
  p <- as.numeric(sample(10, m, replace = TRUE)) / 10
  n <- as.numeric(sample(10, m, replace = TRUE))

  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  expect_equal(
    mapply(monty_random_negative_binomial_prob, n, p, MoreArgs = list(rng)),
    (1 - p) * n / p)
})


test_that("negative binomial prevents bad inputs", {
  r <- monty_rng_create(seed = 1)
  expect_error(monty_random_negative_binomial_prob(10, 0, r),
               "Invalid call to negative_binomial with size = 10, prob = 0")
  expect_error(monty_random_negative_binomial_prob(0, 0.5, r),
               "Invalid call to negative_binomial with size = 0, prob = 0.5")
  expect_error(monty_random_negative_binomial_prob(10, 1.5, r),
               "Invalid call to negative_binomial with size = 10, prob = 1.5")
  expect_error(monty_random_negative_binomial_prob(10, Inf, r),
               "Invalid call to negative_binomial with size = 10, prob = inf")
  expect_error(monty_random_negative_binomial_prob(Inf, 0.4, r),
               "Invalid call to negative_binomial with size = inf, prob = 0.4")
})


test_that("can generate beta-binomial numbers", {
  m <- 1000000
  n <- 100
  a <- 1.5
  b <- 8.5
  r <- monty_rng_create(seed = 1)
  yf <- monty_random_n_beta_binomial_ab(m, n, a, b, r)
  expect_equal(mean(yf), n * a / (a + b), tolerance = 1e-3)
  expect_equal(var(yf),
               n * a * b * (a + b + n) / ((a + b)^2 * (a + b + 1)),
               tolerance = 1e-2)
})


test_that("beta_binomial_prob follows from beta_binomial_ab", {
  rng1 <- monty_rng_create(seed = 1L)
  rng2 <- monty_rng_create(seed = 1L)
  size <- 20
  a <- 2
  b <- 5
  prob <- a / (a + b)
  rho <- 1 / (a + b + 1)
  expect_identical(
    monty_random_beta_binomial_prob(size, prob, rho, rng1),
    monty_random_beta_binomial_ab(size, a, b, rng2))
  expect_identical(
    monty_random_n_beta_binomial_prob(100, size, prob, rho, rng1),
    monty_random_n_beta_binomial_ab(100, size, a, b, rng2))
})


test_that("deterministic beta-binomial returns mean", {
  m <- 100
  n <- as.numeric(sample(100, m, replace = TRUE))
  a <- runif(m, 0, 10)
  b <- runif(m, 0, 10)

  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  expect_equal(
    mapply(monty_random_beta_binomial_ab, n, a, b, MoreArgs = list(rng)),
    n * a / (a + b))
})


test_that("beta-binomial prevents bad inputs", {
  r <- monty_rng_create(seed = 1)
  expect_error(monty_random_beta_binomial_ab(-1, 2, 5, r),
               "Invalid call to beta_binomial with size = -1, a = 2, b = 5")
  expect_error(monty_random_beta_binomial_ab(10, 0, 5, r),
               "Invalid call to beta_binomial with size = 10, a = 0, b = 5")
  expect_error(monty_random_beta_binomial_ab(10, 2, 0, r),
               "Invalid call to beta_binomial with size = 10, a = 2, b = 0")
  expect_error(monty_random_beta_binomial_ab(Inf, 2, 5, r),
               "Invalid call to beta_binomial with size = inf, a = 2, b = 5")
  expect_error(monty_random_beta_binomial_ab(10, Inf, 5, r),
               "Invalid call to beta_binomial with size = 10, a = inf, b = 5")
  expect_error(monty_random_beta_binomial_ab(10, 2, Inf, r),
               "Invalid call to beta_binomial with size = 10, a = 2, b = inf")
})


test_that("can generate samples from the cauchy distribution", {
  ## This one is really hard to validate because the cauchy does not
  ## have any finite moments...
  n <- 100000
  ans <- monty_random_n_cauchy(n, 0, 1, monty_rng_create(seed = 2))
  expect_gt(ks.test(ans, "pcauchy")$p.value, 0.3)
  expect_equal(median(ans), 0, tolerance = 0.01)
})


test_that("deterministic cauchy throws", {
  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  expect_error(
    monty_random_cauchy(0, 1, rng),
    "Can't use Cauchy distribution deterministically; it has no mean")
})


test_that("cauchy density is correct", {
  expect_equal(
    eval(distr_cauchy$expr$density, list(x = 1:10, scale = 2, location = 3)),
    dcauchy(1:10, scale = 2, location = 3, log = TRUE))
})


test_that("regression tests of binomial issues", {
  rng <- monty_rng_create(seed = 42)
  ## all values 0..n represented:
  expect_setequal(monty_random_n_binomial(10000, 1, 0.5, rng), 0:1)
  expect_setequal(monty_random_n_binomial(10000, 4, 0.2, rng), 0:4)
  expect_setequal(monty_random_n_binomial(10000, 10, 0.5, rng), 0:10)
})


test_that("can fetch rng state", {
  set.seed(1)
  expect_false(is.null(get_r_rng_state()))
  rm(list = ".Random.seed", envir = .GlobalEnv)
  expect_true(is.null(get_r_rng_state()))
})


test_that("exponential by mean agrees with rate", {
  ans1 <- monty_random_n_exponential_rate(100, 0.5, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_exponential_mean(100, 2, monty_rng_create(seed = 1))
  expect_equal(ans1, ans2)
})


test_that("can sample from the beta distribution", {
  rng <- monty_rng_create(seed = 42)
  a <- 2.5
  b <- 1.5
  r <- monty_random_n_beta(1000000, a, b, rng)
  expect_equal(mean(r), a / (a + b), tolerance = 1e-3)
  expect_equal(var(r), a * b / ((a + b)^2 * (a + b + 1)), tolerance = 1e-2)
})


test_that("beta generation algorithm is correct", {
  rng1 <- monty_rng_create(seed = 42)
  rng2 <- monty_rng_create(seed = 42)
  a <- 2.5
  b <- 1.5
  r <- monty_random_beta(a, b, rng1)

  x <- monty_random_gamma_scale(a, 1, rng2)
  y <- monty_random_gamma_scale(b, 1, rng2)
  expect_equal(r, x / (x + y))
})


test_that("deterministic beta returns mean", {
  n <- 10
  a <- rexp(n)
  b <- rexp(n)

  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)

  expect_equal(
    mapply(monty_random_beta, a, b, MoreArgs = list(rng)),
    a / (a + b))

  expect_equal(monty_rng_state(rng), state)
})


test_that("can generate from truncated normal", {
  set.seed(1)
  min <- -1
  max <- 2
  r <- monty_rng_create(seed = 1)

  res <- replicate(10, {
    cmp <- rnorm(10000)
    cmp <- cmp[cmp >= min & cmp <= max]
    res <- monty_random_n_truncated_normal(10000, 0, 1, min, max, r)
    suppressWarnings(ks.test(res, cmp)$p.value)
  })
  expect_gt(sum(res > 0.05), 5)
})


test_that("can generate from truncated normal, 1 sided", {
  set.seed(1)
  min <- -1
  max <- 2
  r <- monty_rng_create(seed = 1)

  min <- -1
  max <- Inf
  res <- replicate(10, {
    cmp <- rnorm(10000)
    cmp <- cmp[cmp >= min & cmp <= max]
    res <- monty_random_n_truncated_normal(10000, 0, 1, min, max, r)
    suppressWarnings(ks.test(res, cmp)$p.value)
  })
  expect_gt(sum(res > 0.05), 5)
})


test_that("can handle other one sided distribution", {
  min <- -1
  max <- 2
  r1 <- monty_rng_create(seed = 42)
  r2 <- monty_rng_create(seed = 42)

  min <- -1
  max <- Inf
  y1 <- monty_random_n_truncated_normal(10, 2, 3, -1, Inf, r1)
  y2 <- monty_random_n_truncated_normal(10, -2, 3, -Inf, 1, r2)
  expect_equal(y2, -y1)
})


test_that("can sample untruncated normals from truncated normal", {
  r1 <- monty_rng_create(seed = 42)
  r2 <- monty_rng_create(seed = 42)
  y1 <- monty_random_n_truncated_normal(10, 2, 3, -Inf, Inf, r1)
  y2 <- monty_random_n_normal(10, 2, 3, r2)
  expect_identical(y1, y2)
})


test_that("can compute mean of truncated normal", {
  mean <- 2
  sd <- 3
  min <- -1
  max <- 2
  r <- monty_rng_create(seed = 42, deterministic = TRUE)
  s0 <- monty_rng_state(r)
  y <- monty_random_truncated_normal(mean, sd, min, max, r)
  num <- integrate(function(x) x * dnorm(x, mean, sd), min, max)$value
  den <- integrate(function(x) dnorm(x, mean, sd), min, max)$value
  expect_equal(y, num / den)
  expect_equal(monty_rng_state(r), s0)
})


test_that("can generate from truncated normal from tails", {
  set.seed(1)
  min <- 2
  max <- 4
  r <- monty_rng_create(seed = 1)

  res <- replicate(10, {
    cmp <- rnorm(10000)
    cmp <- cmp[cmp >= min & cmp <= max]
    res <- monty_random_n_truncated_normal(10000, 0, 1, min, max, r)
    suppressWarnings(ks.test(res, cmp)$p.value)
  })
  expect_gt(sum(res > 0.05), 5)
})


test_that("can generate from truncated normal from lower tail", {
  set.seed(1)
  min <- -6
  max <- -1
  r <- monty_rng_create(seed = 1)

  res <- replicate(10, {
    cmp <- rnorm(10000)
    cmp <- cmp[cmp >= min & cmp <= max]
    res <- monty_random_n_truncated_normal(10000, 0, 1, min, max, r)
    suppressWarnings(ks.test(res, cmp)$p.value)
  })
  expect_gt(sum(res > 0.05), 5)
})


test_that("can draw weibull random numbers", {
  shape <- 5
  scale <- 3
  n <- 10000000

  ans1 <- monty_random_n_weibull(n, shape, scale, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_weibull(n, shape, scale, monty_rng_create(seed = 1))
  expect_identical(ans1, ans2)

  expect_equal(mean(ans1), scale * gamma(1 + 1 / shape), tolerance = 1e-3)
  true_var <- scale^2 * (gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2)
  expect_equal(var(ans1), true_var, tolerance = 1e-3)
})


test_that("deterministic weibull returns mean", {
  n_reps <- 10
  shape <- as.numeric(sample(10, n_reps, replace = TRUE))
  scale <- as.numeric(sample(10, n_reps, replace = TRUE))

  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)

  expect_equal(
    mapply(monty_random_weibull, shape, scale, MoreArgs = list(rng)),
    scale * gamma(1 + 1 / shape))
  expect_equal(monty_rng_state(rng), state)
})


test_that("weibull random numbers prevent bad inputs", {
  r <- monty_rng_create(seed = 1)
  expect_equal(monty_random_weibull(0, 0, r), 0)
  expect_equal(monty_random_weibull(Inf, Inf, r), Inf)

  expect_error(
    monty_random_weibull(-1.1, 5.1, r),
    "Invalid call to Weibull with shape = -1.1, scale = 5.1")
  expect_error(
    monty_random_weibull(5.1, -1.1, r),
    "Invalid call to Weibull with shape = 5.1, scale = -1.1")
})


test_that("can draw log-normal random numbers", {
  meanlog <- 1.5
  sdlog <- 0.5
  n <- 10000000

  ans1 <-
    monty_random_n_log_normal(n, meanlog, sdlog, monty_rng_create(seed = 1))
  ans2 <-
    monty_random_n_log_normal(n, meanlog, sdlog, monty_rng_create(seed = 1))
  expect_identical(ans1, ans2)

  expect_equal(mean(ans1), exp(meanlog + sdlog^2 / 2), tolerance = 1e-3)
  true_var <- (exp(sdlog^2) - 1) * exp(2 * meanlog + sdlog^2)
  expect_equal(var(ans1), true_var, tolerance = 1e-2)
})


test_that("deterministic log-normal returns mean", {
  n_reps <- 10
  meanlog <- as.numeric(sample(seq(-10, 10), n_reps, replace = TRUE))
  sdlog <- as.numeric(sample(10, n_reps, replace = TRUE))

  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)

  expect_equal(
    mapply(monty_random_log_normal, meanlog, sdlog, MoreArgs = list(rng)),
    exp(meanlog + sdlog^2 / 2))
  expect_equal(monty_rng_state(rng), state)
})


test_that("log-normal random numbers prevent bad inputs", {
  r <- monty_rng_create(seed = 1)

  expect_error(
    monty_random_log_normal(1.1, -5.1, r),
    "Invalid call to log_normal with meanlog = 1.1, sdlog = -5.1")
})


test_that("can draw zero-inflated poisson random numbers", {
  lambda <- 5
  pi <- 0.2
  n <- 10000000
  
  ans1 <- monty_random_n_zi_poisson(n, lambda, pi, monty_rng_create(seed = 1))
  ans2 <- monty_random_n_zi_poisson(n, lambda, pi, monty_rng_create(seed = 1))
  expect_identical(ans1, ans2)
  
  expect_equal(mean(ans1), (1 - pi) * lambda, tolerance = 1e-3)
  true_var <- lambda * (1 - pi) * (1 + pi * lambda)
  expect_equal(var(ans1), true_var, tolerance = 1e-3)
})


test_that("deterministic zero-inflated poisson returns mean", {
  n_reps <- 10
  lambda <- as.numeric(sample(10, n_reps, replace = TRUE))
  pi <- as.numeric(sample(10, n_reps, replace = TRUE) / 10)
  
  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  state <- monty_rng_state(rng)
  
  expect_equal(
    mapply(monty_random_zi_poisson, lambda, pi, MoreArgs = list(rng)),
    (1 - pi) * lambda)
  expect_equal(monty_rng_state(rng), state)
})


test_that("Zero-inflated poisson random numbers prevent bad inputs", {
  r <- monty_rng_create(seed = 1)
  expect_equal(monty_random_zi_poisson(5, 1, r), 0)
  expect_equal(monty_random_zi_poisson(0, 0.5, r), 0)
  
  expect_error(
    monty_random_zi_poisson(-1.1, 0.1, r),
    "Invalid call to zi_poisson with lambda = -1.1, pi = 0.1")
  expect_error(
    monty_random_zi_poisson(Inf, 0.1, r),
    "Invalid call to zi_poisson with lambda = inf, pi = 0.1")
  expect_error(
    monty_random_zi_poisson(5, -0.1, r),
    "Invalid call to zi_poisson with lambda = 5, pi = -0.1")
  expect_error(
    monty_random_zi_poisson(5, 1.1, r),
    "Invalid call to zi_poisson with lambda = 5, pi = 1.1")
})


test_that("can generate zero-inflated negative binomial numbers", {
  m <- 1000000
  n <- 958
  p <- 0.004145
  pi <- 0.2
  r <- monty_rng_create(seed = 1)
  yf <- monty_random_n_zi_negative_binomial_prob(m, n, p, pi, r)
  
  negbin_mean <- (1 - p) * n / p
  true_mean <- (1 - pi) * 
  expect_equal(mean(yf), (1 - pi) * negbin_mean, tolerance = 1e-3)
  true_var <- (1 - pi) * negbin_mean * (1 + negbin_mean * (pi + 1 / n))
  expect_equal(var(yf), true_var, tolerance = 1e-2)
})


test_that("zi_negative_binomial_mu follows from zi_negative_binomial_prob", {
  rng1 <- monty_rng_create(seed = 1L)
  rng2 <- monty_rng_create(seed = 1L)
  prob <- 0.3
  size <- 20
  pi <- 0.2
  mu <- size * (1 - prob) / prob
  expect_identical(
    monty_random_zi_negative_binomial_mu(size, mu, pi, rng1),
    monty_random_zi_negative_binomial_prob(size, prob, pi, rng2))
  expect_identical(
    monty_random_n_zi_negative_binomial_mu(100, size, mu, pi, rng1),
    monty_random_n_zi_negative_binomial_prob(100, size, prob, pi, rng2))
})


test_that("deterministic zero-inflated negative binomial returns mean", {
  m <- 100
  p <- as.numeric(sample(10, m, replace = TRUE)) / 10
  pi <- as.numeric(sample(10, m, replace = TRUE)) / 10
  n <- as.numeric(sample(10, m, replace = TRUE))
  
  rng <- monty_rng_create(seed = 1, deterministic = TRUE)
  expect_equal(
    mapply(monty_random_zi_negative_binomial_prob, n, p, pi,
           MoreArgs = list(rng)),
    (1 - pi) * (1 - p) * n / p)
})


test_that("negative binomial prevents bad inputs", {
  r <- monty_rng_create(seed = 1)
  expect_error(monty_random_zi_negative_binomial_prob(10, 0, 0.2, r),
               "Invalid call to zi_negative_binomial")
  expect_error(monty_random_zi_negative_binomial_prob(0, 0.5, 0.2, r),
               "Invalid call to zi_negative_binomial")
  expect_error(monty_random_zi_negative_binomial_prob(10, 1.5, 0.2, r),
               "Invalid call to zi_negative_binomial")
  expect_error(monty_random_zi_negative_binomial_prob(10, Inf, 0.2, r),
               "Invalid call to zi_negative_binomial")
  expect_error(monty_random_zi_negative_binomial_prob(Inf, 0.4, 0.2, r),
               "Invalid call to zi_negative_binomial")
  expect_error(monty_random_zi_negative_binomial_prob(10, 0.4, -0.2, r),
               "Invalid call to zi_negative_binomial")
  expect_error(monty_random_zi_negative_binomial_prob(10, 0.4, 1.2, r),
               "Invalid call to zi_negative_binomial")
})
