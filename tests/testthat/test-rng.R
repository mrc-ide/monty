test_that("can generate random numbers", {
  ans1 <- monty_rng$new(1)$random_real(100)
  ans2 <- monty_rng$new(1)$random_real(100)
  ans3 <- monty_rng$new(2)$random_real(100)
  expect_equal(length(ans1), 100)
  expect_identical(ans1, ans2)
  expect_false(any(ans1 == ans3))
})


test_that("Create interleaved rng", {
  n <- 128
  seed <- 1

  rng1 <- monty_rng$new(seed, 1L)
  rng2 <- monty_rng$new(seed, 2L)
  rng3 <- monty_rng$new(seed, 4L)
  rng4 <- monty_rng$new(seed, 8L)

  ans1 <- rng1$random_real(n)
  ans2 <- rng2$random_real(n)
  ans3 <- rng3$random_real(n)
  ans4 <- rng4$random_real(n)

  ## We can find elements from the each rng through the larger
  ## sequences:
  expect_identical(ans1, ans2[, 1])
  expect_identical(ans1, ans3[, 1])
  expect_identical(ans1, ans3[, 1])
  expect_identical(ans2, ans3[, 1:2])
  expect_identical(ans2, ans4[, 1:2])
  expect_identical(ans3, ans4[, 1:4])

  expect_equal(rng1$size(), 1)
  expect_equal(rng2$size(), 2)
  expect_equal(rng3$size(), 4)
  expect_equal(rng4$size(), 8)
})


test_that("run uniform random numbers", {
  ans1 <- monty_rng$new(1L)$random_real(100)
  ans2 <- monty_rng$new(1L)$random_real(100)
  ans3 <- monty_rng$new(1L)$uniform(100, 0, 1)
  ans4 <- monty_rng$new(2L)$uniform(100, 0, 1)

  expect_true(all(ans1 >= 0))
  expect_true(all(ans1 <= 1))
  expect_identical(ans1, ans2)
  expect_identical(ans1, ans3)
  expect_false(any(ans1 == ans4))
})


test_that("run uniform random numbers with odd bounds", {
  ans <- monty_rng$new(1L)$uniform(100, -100, 100)
  expect_true(any(ans > 0))
  expect_true(any(ans < 0))
  expect_true(all(ans >= -100))
  expect_true(all(ans <= 100))
})


test_that("distribution of uniform numbers", {
  m <- 100000
  a <- exp(1)
  b <- pi
  ans <- monty_rng$new(1)$uniform(m, a, b)
  expect_equal(mean(ans), (a + b) / 2, tolerance = 1e-3)
  expect_equal(var(ans), (b - a)^2 / 12, tolerance = 1e-2)
})


test_that("run binomial random numbers", {
  m <- 100000
  n <- 100
  p <- 0.1

  ans1 <- monty_rng$new(1)$binomial(m, n, p)
  ans2 <- monty_rng$new(1)$binomial(m, n, p)
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

  expect_identical(monty_rng$new(1)$binomial(m, 0, p),
                   rep(0, m))
  expect_identical(monty_rng$new(1)$binomial(m, n, 0),
                   rep(0, m))
  expect_identical(monty_rng$new(1)$binomial(m, n, 1),
                   rep(as.numeric(n), m))
})


test_that("binomial numbers on the 'small' path", {
  m <- 500000
  n <- 20
  p <- 0.2

  ans1 <- monty_rng$new(1)$binomial(m, n, p)
  expect_equal(mean(ans1), n * p, tolerance = 1e-3)
  expect_equal(var(ans1), n * p * (1 - p), tolerance = 1e-2)
})


test_that("binomial numbers and their complement are the same (np small)", {
  m <- 100
  n <- 20
  p <- 0.2

  ans1 <- monty_rng$new(1)$binomial(m, n, p)
  ans2 <- monty_rng$new(1)$binomial(m, n, 1 - p)
  expect_equal(ans1, n - ans2)
})


test_that("binomial numbers and their complement are the same (np large)", {
  m <- 100
  n <- 200
  p <- 0.2

  ans1 <- monty_rng$new(1)$binomial(m, n, p)
  ans2 <- monty_rng$new(1)$binomial(m, n, 1 - p)
  expect_equal(ans1, n - ans2)
})


test_that("Binomial random numbers prevent bad inputs", {
  skip_on_cran() # potentially system dependent
  r <- monty_rng$new(1)
  r$binomial(1, 0, 0)
  expect_error(
    r$binomial(1, 1, -1),
    "Invalid call to binomial with n = 1, p = -1")
  expect_error(
    r$binomial(1, 1, 0 - 1e-8),
    "Invalid call to binomial with n = 1, p = -1e-08")
  expect_error(
    r$binomial(1, 1, 2),
    "Invalid call to binomial with n = 1, p = 2")
  ## TODO: this is not a great error here, but there's not much that
  ## can be done without a lot of faff with the underlying print
  expect_error(
    r$binomial(1, 1, 1 + 1e-8),
    "Invalid call to binomial with n = 1, p = 1")
  expect_error(
    r$binomial(1, -1, 0.5),
    "Invalid call to binomial with n = -1, p = 0.5")
  expect_error(
    r$binomial(1, 1, NaN),
    "Invalid call to binomial with n = 1, p = .+")
  expect_error(
    r$binomial(1, NaN, 1),
    "Invalid call to binomial with n = .+, p = 1")
})


test_that("avoid integer overflow in binomial draws with very large n", {
  r <- monty_rng$new(1, seed = 1L)
  n <- 2^33
  expect_equal(r$binomial(1, n, 0), 0)
  expect_equal(r$binomial(1, n, 1), n)

  p1 <- 2.5e-10
  m1 <- r$binomial(1000000, n, p1)
  expect_equal(mean(m1), n * p1, tolerance = 1e-3)
  mt1 <- tabulate(m1)
  expect_false(any(mt1) == 0)

  p2 <- 0.1
  m2 <- r$binomial(1000000, n, p2)
  mt2 <- tabulate(m2)
  expect_equal(mean(m2), n * p2, tolerance = 1e-3)
  expect_false(any(mt1) == 0)
})


test_that("poisson numbers", {
  n <- 100000
  lambda <- 5

  ans1 <- monty_rng$new(1)$poisson(n, lambda)
  ans2 <- monty_rng$new(1)$poisson(n, lambda)
  ans3 <- monty_rng$new(2)$poisson(n, lambda)
  expect_identical(ans1, ans2)
  expect_false(all(ans1 == ans3))

  expect_equal(mean(ans1), lambda, tolerance = 1e-2)
  expect_equal(var(ans1), lambda, tolerance = 1e-2)
})


test_that("Big poisson numbers", {
  n <- 100000
  lambda <- 20

  ans1 <- monty_rng$new(1)$poisson(n, lambda)
  ans2 <- monty_rng$new(1)$poisson(n, lambda)
  ans3 <- monty_rng$new(2)$poisson(n, lambda)
  expect_identical(ans1, ans2)
  expect_false(all(ans1 == ans3))

  expect_equal(mean(ans1), lambda, tolerance = 1e-2)
  expect_equal(var(ans1), lambda, tolerance = 1e-2)
})


test_that("big poisson numbers at edge of transition are ok", {
  n <- 100000
  lambda <- 1e8 - 1
  rng <- monty_rng$new(1)
  ans <- rng$poisson(n, lambda)
  expect_equal(mean(ans), lambda, tolerance = 1e-2)
  expect_equal(var(ans), lambda, tolerance = 1e-2)
})


test_that("Very big poisson numbers", {
  n <- 100000
  lambda <- 1e12

  ans1 <- monty_rng$new(1)$poisson(n, lambda)
  ans2 <- monty_rng$new(1)$poisson(n, lambda)
  ans3 <- monty_rng$new(2)$poisson(n, lambda)
  expect_identical(ans1, ans2)
  expect_false(all(ans1 == ans3))

  expect_equal(mean(ans1), lambda, tolerance = 1e-2)
  expect_equal(var(ans1), lambda, tolerance = 1e-2)
})


test_that("Poisson numbers only valid for 0 <= lambda < Inf", {
  n <- 100
  expect_error(monty_rng$new(1)$poisson(n, -1),
               "Invalid call to Poisson")
  expect_error(monty_rng$new(1)$poisson(n, Inf),
               "Invalid call to Poisson")
})


test_that("Short circuit exit does not update rng state", {
  rng <- monty_rng$new(1)
  s <- rng$state()
  ans <- rng$poisson(100, 0)
  expect_equal(ans, rep(0, 100))
  expect_identical(rng$state(), s)
})


test_that("normal (box_muller) agrees with stats::rnorm", {
  n <- 100000
  ans <- monty_rng$new(2)$random_normal(n)
  expect_equal(mean(ans), 0, tolerance = 1e-2)
  expect_equal(sd(ans), 1, tolerance = 1e-2)
  expect_gt(ks.test(ans, "pnorm")$p.value, 0.1)
})


test_that("normal (polar) agrees with stats::rnorm", {
  n <- 100000
  ans <- monty_rng$new(2)$random_normal(n, algorithm = "polar")
  expect_equal(mean(ans), 0, tolerance = 1e-2)
  expect_equal(sd(ans), 1, tolerance = 1e-2)
  expect_gt(ks.test(ans, "pnorm")$p.value, 0.1)
})


test_that("normal (ziggurat) agrees with stats::rnorm", {
  n <- 100000
  ans <- monty_rng$new(2)$random_normal(n, algorithm = "ziggurat")
  expect_equal(mean(ans), 0, tolerance = 1e-2)
  expect_equal(sd(ans), 1, tolerance = 1e-2)
  expect_gt(ks.test(ans, "pnorm")$p.value, 0.1)
})


test_that("normal scales draws", {
  n <- 100
  mean <- exp(1)
  sd <- pi
  rng1 <- monty_rng$new(1)
  rng2 <- monty_rng$new(1)
  expect_equal(rng1$normal(n, mean, sd),
               mean + sd * rng2$random_normal(n))
  expect_equal(rng1$normal(n, mean, sd, algorithm = "polar"),
               mean + sd * rng2$random_normal(n, algorithm = "polar"))
  expect_equal(rng1$normal(n, mean, sd, algorithm = "ziggurat"),
               mean + sd * rng2$random_normal(n, algorithm = "ziggurat"))
})


test_that("Prevent unknown normal algorithms", {
  expect_error(
    monty_rng$new(2)$random_normal(10, algorithm = "monty_python"),
    "Unknown normal algorithm 'monty_python'")
  expect_error(
    monty_rng$new(2)$normal(10, 0, 1, algorithm = "monty_python"),
    "Unknown normal algorithm 'monty_python'")
})


test_that("rexp agrees with stats::rexp", {
  n <- 100000
  rate <- 0.04
  ans <- monty_rng$new(2)$exponential_rate(n, rate)
  expect_equal(mean(ans), 1 / rate, tolerance = 1e-2)
  expect_equal(var(ans), 1 / rate^2, tolerance = 1e-2)
  expect_gt(ks.test(ans, "pexp", rate)$p.value, 0.1)
})


test_that("continue stream", {
  rng1 <- monty_rng$new(1)
  rng2 <- monty_rng$new(1)

  y1 <- rng1$uniform(100, 0, 1)
  y2_1 <- rng2$uniform(50, 0, 1)
  y2_2 <- rng2$uniform(50, 0, 1)
  y2 <- c(y2_1, y2_2)
  expect_identical(y1, y2)
})


test_that("jump", {
  seed <- 1
  rng1a <- monty_rng$new(seed)
  rng1b <- monty_rng$new(seed)$jump()
  rng2 <- monty_rng$new(seed, 2L)

  r2 <- rng2$random_real(10)
  r1a <- rng1a$random_real(10)
  r1b <- rng1b$random_real(10)

  expect_equal(cbind(r1a, r1b, deparse.level = 0), r2)
})


test_that("long jump", {
  seed <- 1
  rng1 <- monty_rng$new(seed)
  rng2 <- monty_rng$new(seed)$jump()
  rng3 <- monty_rng$new(seed)$long_jump()
  rng4 <- monty_rng$new(seed)$long_jump()$jump()

  r1 <- rng1$random_real(20)
  r2 <- rng2$random_real(20)
  r3 <- rng3$random_real(20)
  r4 <- rng4$random_real(20)

  expect_true(all(r1 != r2))
  expect_true(all(r1 != r3))
  expect_true(all(r1 != r4))
  expect_true(all(r2 != r3))
  expect_true(all(r2 != r4))
  expect_true(all(r3 != r4))
})


test_that("get state", {
  seed <- 1
  rng1 <- monty_rng$new(seed)
  rng2 <- monty_rng$new(seed)
  rng3 <- monty_rng$new(seed, 2L)

  s1 <- rng1$state()
  expect_type(s1, "raw")
  expect_equal(length(s1), 32)

  s2 <- rng2$state()
  expect_identical(s2, s1)

  s3 <- rng3$state()
  expect_equal(length(s3), 64)
  expect_identical(s3[seq_len(32)], s1)
  expect_identical(s3[-seq_len(32)], rng2$jump()$state())
})


test_that("initialise single rng with binary state", {
  seed <- 42
  rng1 <- monty_rng$new(seed)
  state <- rng1$state()
  rng2 <- monty_rng$new(state)
  expect_identical(rng1$state(), rng2$state())
  r1 <- rng1$random_real(10)
  r2 <- rng2$random_real(10)
  expect_identical(r1, r2)
  expect_identical(rng1$state(), rng2$state())
})


test_that("initialise parallel rng with binary state", {
  seed <- 42
  rng1 <- monty_rng$new(seed, 5L)
  state <- rng1$state()
  rng2 <- monty_rng$new(state, 5L)
  r1 <- rng1$random_real(10)
  r2 <- rng2$random_real(10)
  expect_identical(r1, r2)
  expect_identical(rng1$state(), rng2$state())
})


test_that("initialise parallel rng with single binary state and jump", {
  seed <- 42
  rng1 <- monty_rng$new(seed, 1L)
  rng2 <- monty_rng$new(seed, 2L)
  state <- rng1$state()
  rng3 <- monty_rng$new(state, 2L)
  expect_identical(rng3$state(), rng2$state())
})


test_that("initialise parallel rng with binary state and drop", {
  seed <- 42
  rng10 <- monty_rng$new(seed, 10L)
  rng5 <- monty_rng$new(rng10$state(), 5L)
  len <- 5 * rng5$info$size_state_bytes
  expect_identical(rng5$state(), rng10$state()[seq_len(len)])
})


test_that("require that raw vector is of sensible size", {
  expect_error(monty_rng$new(raw()),
               "Expected raw vector of length as multiple of 32 for 'seed'")
  expect_error(monty_rng$new(raw(31)),
               "Expected raw vector of length as multiple of 32 for 'seed'")
  expect_error(monty_rng$new(raw(63)),
               "Expected raw vector of length as multiple of 32 for 'seed'")
})


test_that("initialise with NULL, generating a seed from R", {
  set.seed(1)
  rng1 <- monty_rng$new(NULL)
  set.seed(1)
  rng2 <- monty_rng$new(NULL)
  rng3 <- monty_rng$new(NULL)
  set.seed(1)

  expect_identical(rng2$state(), rng1$state())
  expect_false(identical(rng3$state(), rng2$state()))
})


test_that("can't create rng with silly things", {
  expect_error(
    monty_rng$new(mtcars),
    "Invalid type for 'seed'")
  expect_error(
    monty_rng$new(function(x) 2),
    "Invalid type for 'seed'")
})


test_that("negative seed values result in sensible state", {
  ## Don't end up with all-zero state, and treat different negative
  ## numbers as different (don't truncate to zero or anything
  ## pathalogical)
  s0 <- monty_rng$new(0)$state()
  s1 <- monty_rng$new(-1)$state()
  s10 <- monty_rng$new(-10)$state()

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

  res <- monty_rng$new(1, seed = 1L)$multinomial(n, size, prob)

  ## Separate implementation of the core algorithm:
  cmp_multinomial <- function(rng, size, prob) {
    p <- prob / (1 - cumsum(c(0, prob[-length(prob)])))
    ret <- numeric(length(prob))
    for (i in seq_len(length(prob) - 1L)) {
      ret[i] <- rng$binomial(1, size, p[i])
      size <- size - ret[i]
    }
    ret[length(ret)] <- size
    ret
  }

  rng2 <- monty_rng$new(1, seed = 1L)
  cmp <- replicate(n, cmp_multinomial(rng2, size, prob))
  expect_equal(res, cmp)
})


test_that("multinomial expectation is correct", {
  p <- runif(10)
  p <- p / sum(p)
  n <- 10000
  res <- monty_rng$new(1, seed = 1L)$multinomial(n, 100, p)
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
  res <- monty_rng$new(1, seed = 1L)$multinomial(n, size, p)

  expect_equal(res[4, ], rep(0, n))
  expect_equal(colSums(res), rep(size, n))
})


test_that("multinomial allows non-normalised prob", {
  p <- runif(10, 0, 10)
  n <- 50
  res1 <- monty_rng$new(1, seed = 1L)$multinomial(n, 100, p)
  res2 <- monty_rng$new(1, seed = 1L)$multinomial(n, 100, p / sum(p))
  expect_equal(res1, res2)
})


test_that("Invalid prob throws an error", {
  r <- monty_rng$new(1, seed = 1L)
  expect_error(
    r$multinomial(1, 10, c(0, 0, 0)),
    "No positive prob in call to multinomial")
  expect_error(
    r$multinomial(1, 10, c(-0.1, 0.6, 0.5)),
    "Negative prob passed to multinomial")
})


test_that("Can vary parameters for multinomial, single generator", {
  np <- 7L
  ng <- 1L
  size <- 13
  n <- 17L
  prob <- matrix(runif(np * n), np, n)
  prob <- prob / rep(colSums(prob), each = np)

  rng <- monty_rng$new(1, seed = 1L)
  cmp <- vapply(seq_len(n), function(i) rng$multinomial(1, size, prob[, i]),
                numeric(np))
  res <- monty_rng$new(1, seed = 1L)$multinomial(n, size, prob)
  expect_equal(res, cmp)

  expect_error(
    monty_rng$new(1, seed = 1L)$multinomial(n, size, prob[, -5]),
    "If 'prob' is a matrix, it must have 17 columns")
  expect_error(
    monty_rng$new(1, seed = 1L)$multinomial(n, size, prob[0, ]),
    "Input parameters imply length of 'prob' of only 0 (< 2)",
    fixed = TRUE)
  expect_error(
    monty_rng$new(1, seed = 1L)$multinomial(n, size, prob[1, , drop = FALSE]),
    "Input parameters imply length of 'prob' of only 1 (< 2)",
    fixed = TRUE)
})


test_that("Can vary parameters by generator for multinomial", {
  np <- 7L
  ng <- 3L
  size <- 13
  n <- 17L

  prob <- array(runif(np * ng), c(np, 1, ng))
  prob <- prob / rep(colSums(prob), each = np)

  state <- matrix(monty_rng$new(ng, seed = 1L)$state(), ncol = ng)
  cmp <- vapply(seq_len(ng), function(i) {
    monty_rng$new(1, seed = state[, i])$multinomial(n, size, prob[, , i])
  }, matrix(numeric(), np, n))

  res <- monty_rng$new(ng, seed = 1L)$multinomial(n, size, prob)
  expect_equal(res, cmp)
})


test_that("Can vary parameters for multinomial, multiple generators", {
  np <- 7L
  ng <- 3L
  size <- 13
  n <- 17L
  prob <- array(runif(np * n * ng), c(np, n, ng))
  prob <- prob / rep(colSums(prob), each = np)

  ## Setting up the expectation here is not easy, we need a set of
  ## generators. This test exploits the fact that we alredy worked out
  ## we could vary a parameter over draws with a single generator.
  state <- matrix(monty_rng$new(ng, seed = 1L)$state(), ncol = ng)
  cmp <- vapply(seq_len(ng), function(i) {
    monty_rng$new(1, seed = state[, i])$multinomial(n, size, prob[, , i])
  }, matrix(numeric(), np, n))

  res <- monty_rng$new(ng, seed = 1L)$multinomial(n, size, prob)
  expect_equal(res, cmp)

  expect_error(
    monty_rng$new(ng, seed = 1L)$multinomial(n, size, prob[, -5, ]),
    "If 'prob' is a 3d array, it must have 1 or 17 columns")
  expect_error(
    monty_rng$new(ng, seed = 1L)$multinomial(n, size, prob[, , -1]),
    "If 'prob' is a 3d array, it must have 3 layers")
  expect_error(
    monty_rng$new(ng, seed = 1L)$multinomial(n, size, prob[0, , ]),
    "Input parameters imply length of 'prob' of only 0 (< 2)",
    fixed = TRUE)
  ## Final bad inputs:
  p4 <- array(prob, c(dim(prob), 1))
  expect_error(
    monty_rng$new(ng, seed = 1L)$multinomial(n, size, p4),
    "'prob' must be a vector, matrix or 3d array")
})


test_that("deterministic rbinom returns mean", {
  m <- 10
  n <- as.numeric(sample(10, m, replace = TRUE))
  p <- runif(m)

  rng <- monty_rng$new(1, deterministic = TRUE)
  state <- rng$state()

  expect_equal(rng$binomial(m, n, p), n * p)

  expect_equal(rng$state(), state)
})


test_that("deterministic rbinom accepts non-integer size", {
  m <- 10
  n <- runif(m, 0, 10)
  p <- runif(m)
  rng <- monty_rng$new(1, deterministic = TRUE)
  state <- rng$state()
  expect_equal(rng$binomial(m, n, p), n * p)
  expect_equal(rng$state(), state)
})


test_that("deterministic rbinom allow small negative innacuracies", {
  m <- 10
  n <- runif(m, 0, 10)
  p <- runif(m)
  rng <- monty_rng$new(1, deterministic = TRUE)

  eps <- .Machine$double.eps

  expect_identical(rng$binomial(1, 0, 0.5), 0.0)
  expect_identical(rng$binomial(1, -eps, 0.5), 0.0)

  expect_error(rng$binomial(1, -sqrt(eps * 1.1), 0.5),
               "Invalid call to binomial with n = -")
})


test_that("deterministic rpois returns mean", {
  m <- 10
  ## numbers from all three regimes:
  lambda <- c(
    runif(m, 0, 10),
    runif(m, 10, 1000),
    runif(m, 1e10, 1e12))
  rng <- monty_rng$new(1, deterministic = TRUE)
  state <- rng$state()
  expect_equal(rng$poisson(3 * m, lambda), lambda)
  expect_equal(rng$state(), state)
})


test_that("deterministic runif returns mean", {
  m <- 10
  l <- runif(m, -10, 10)
  u <- l + runif(m, 0, 10)
  rng <- monty_rng$new(1, deterministic = TRUE)
  state <- rng$state()
  expect_equal(rng$uniform(m, l, u), (l + u) / 2)
  expect_equal(rng$state(), state)
})


test_that("deterministic rexp returns mean", {
  m <- 10
  rate <- runif(m, 0, 10)
  rng <- monty_rng$new(1, deterministic = TRUE)
  state <- rng$state()
  expect_equal(rng$exponential_rate(m, rate), 1 / rate)
  expect_equal(rng$state(), state)
})


test_that("deterministic rnorm returns mean", {
  m <- 10
  mu <- runif(m, -10, 10)
  sd <- runif(m, 0, 10)
  rng <- monty_rng$new(1, deterministic = TRUE)
  state <- rng$state()
  expect_equal(rng$normal(m, mu, sd), mu)
  expect_equal(rng$state(), state)
})


test_that("Parameter expansion", {
  rng <- monty_rng$new(1, 10)

  m <- matrix(as.numeric(1:30), 3, 10)
  rng <- monty_rng$new(1, 10)
  expect_equal(floor(rng$uniform(3, m, m + 1)), m)

  expect_equal(floor(rng$uniform(3, m[, 1], m[, 1] + 1)),
               matrix(as.numeric(1:3), 3, 10))
  expect_equal(floor(rng$uniform(3, 1, 2)),
               matrix(1, 3, 10))
  m1 <- m[1, , drop = FALSE]
  expect_equal(floor(rng$uniform(3, m1, m1 + 1)),
               m1[c(1, 1, 1), ])

  expect_error(
    rng$uniform(3, c(1, 2, 3, 4), 10),
    "If 'min' is a vector, it must have 1 or 3 elements")
  expect_error(
    rng$uniform(3, m[, 1:2], 10),
    "If 'min' is a matrix, it must have 10 columns")
  expect_error(
    rng$uniform(3, m[1:2, ], 10),
    "If 'min' is a matrix, it must have 1 or 3 rows")
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
  cmp <- monty_rng$new(42)$normal(10, 0, 1)
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

  cmp <- colSums(monty_rng$new(42, 5)$uniform(10, 0, 1))
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
  r1 <- monty_rng$new(seed = 1)
  r2 <- monty_rng$new(seed = 1)
  hypergeometric <- hypergeometric_r(function() r1$random_real(1))
  n1 <- 4334282
  n2 <- 5665718
  size <- 750
  ans1 <- replicate(100, hypergeometric(n1, n2, size))
  ans2 <- r2$hypergeometric(100, n1, n2, size)
  expect_equal(ans1, ans2)
})


test_that("rng agrees with hypergeometric reference implementation", {
  rng1 <- monty_rng$new(seed = 1L)
  rng2 <- monty_rng$new(seed = 1L)
  hypergeometric <- hypergeometric_r(function() rng1$random_real(1))

  ## Same three cases as above:
  ## Case 1, use HIP (inversion) algorithm
  m <- 7
  n <- 10
  k <- 8
  r1 <- replicate(500, hypergeometric(m, n, k))
  r2 <- rng2$hypergeometric(500, m, n, k)
  expect_equal(r1, r2)

  ## Case 1a, HIP but sample exactly half
  m <- 5
  n <- 5
  k <- 5
  r1 <- replicate(500, hypergeometric(m, n, k))
  r2 <- rng2$hypergeometric(500, m, n, k)
  expect_equal(r1, r2)

  ## Case 2, use H2PE algorithm, simple exit
  m <- 70
  n <- 100
  k <- 80
  r1 <- replicate(500, hypergeometric(m, n, k))
  r2 <- rng2$hypergeometric(500, m, n, k)
  expect_equal(r1, r2)

  ## Case 3, use H2PE algorithm, squeezing exit
  m <- 700
  n <- 1000
  k <- 800
  r1 <- replicate(500, hypergeometric(m, n, k))
  r2 <- rng2$hypergeometric(500, m, n, k)
  expect_equal(r1, r2)
})


test_that("symmetry property around n1/n2 and k holds", {
  n1 <- 7
  n2 <- 15
  k <- 5
  n <- n1 + n2
  r1 <- monty_rng$new(seed = 1L)$hypergeometric(500, n1, n2, k)
  r2 <- monty_rng$new(seed = 1L)$hypergeometric(500, n2, n1, k)
  r3 <- monty_rng$new(seed = 1L)$hypergeometric(500, n1, n2, n - k)
  r4 <- monty_rng$new(seed = 1L)$hypergeometric(500, n2, n1, n - k)
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

  rng <- monty_rng$new(1, deterministic = TRUE)
  state <- rng$state()

  expect_equal(rng$hypergeometric(n_reps, n1, n2, k), k * n1 / n)

  expect_equal(rng$state(), state)
})


test_that("hypergeometric random numbers prevent bad inputs", {
  r <- monty_rng$new(1)
  expect_equal(r$hypergeometric(1, 0, 0, 0), 0)

  expect_error(
    r$hypergeometric(1, -1, 5, 2),
    "Invalid call to hypergeometric with n1 = -1, n2 = 5, k = 2")
  expect_error(
    r$hypergeometric(1, 5, -1, 2),
    "Invalid call to hypergeometric with n1 = 5, n2 = -1, k = 2")
  expect_error(
    r$hypergeometric(1, 5, 3, -2),
    "Invalid call to hypergeometric with n1 = 5, n2 = 3, k = -2")
  expect_error(
    r$hypergeometric(1, 5, 3, 10),
    "Invalid call to hypergeometric with n1 = 5, n2 = 3, k = 10")
})


test_that("fast exits do not draw random numbers", {
  r <- monty_rng$new(1)
  s <- r$state()

  ## If there's nothing sampled from nothing, return nothing
  expect_equal(r$hypergeometric(1, 0, 0, 0), 0)
  ## If there's nothing sampled from something, return nothing
  expect_equal(r$hypergeometric(1, 10, 5, 0), 0)
  ## If there's nothing to choose from, take the only option
  expect_equal(r$hypergeometric(1, 10, 0, 2), 2)
  expect_equal(r$hypergeometric(1, 0, 10, 2), 0)
  ## If we select everything, return everything
  expect_equal(r$hypergeometric(1, 10, 5, 15), 10)
  expect_equal(r$hypergeometric(1, 5, 10, 15), 5)

  expect_identical(r$state(), s)
})


test_that("numbers on different streams behave as expected", {
  r <- monty_rng$new(2, seed = 1)
  m <- 9
  n <- 3
  k <- 7
  res <- r$hypergeometric(10, m, n, k)
  expect_equal(dim(res), c(10, 2))
  expect_equal(res[, 1],
               monty_rng$new(1, seed = 1)$hypergeometric(10, m, n, k))
  expect_equal(res[, 2],
               monty_rng$new(1, seed = 1)$jump()$hypergeometric(10, m, n, k))
})


test_that("gamma for a = 1 is the same as exponential", {
  rng1 <- monty_rng$new(seed = 1L)
  rng2 <- monty_rng$new(seed = 1L)

  n <- 10
  b <- 3

  gamma <- rng1$gamma_scale(n, 1, b)
  exp <- rng2$exponential_rate(n, 1 / b)

  expect_equal(gamma, exp)
})


test_that("gamma_rate follows from gamma_scale", {
  rng1 <- monty_rng$new(seed = 1L)
  rng2 <- monty_rng$new(seed = 1L)
  b <- 3
  expect_identical(
    rng1$gamma_rate(100, 5, 1 / b),
    rng2$gamma_scale(100, 5, b))
})


test_that("can draw gamma random numbers", {
  ## when a > 1
  a <- 5
  b <- 3
  n <- 10000000

  ans1 <- monty_rng$new(1)$gamma_scale(n, a, b)
  ans2 <- monty_rng$new(1)$gamma_scale(n, a, b)
  expect_identical(ans1, ans2)

  expect_equal(mean(ans1), a * b, tolerance = 1e-3)
  expect_equal(var(ans1), a * b^2, tolerance = 1e-3)

  ## when a < 1
  a <- 0.5

  ans3 <- monty_rng$new(1)$gamma_scale(n, a, b)
  ans4 <- monty_rng$new(1)$gamma_scale(n, a, b)
  expect_identical(ans3, ans4)

  expect_equal(mean(ans3), a * b, tolerance = 1e-3)
  expect_equal(var(ans3), a * b^2, tolerance = 1e-3)
})


test_that("deterministic gamma returns mean", {
  n_reps <- 10
  a <- as.numeric(sample(10, n_reps, replace = TRUE))
  b <- as.numeric(sample(10, n_reps, replace = TRUE))

  rng <- monty_rng$new(1, deterministic = TRUE)
  state <- rng$state()

  expect_equal(rng$gamma_scale(n_reps, a, b), a * b)

  expect_equal(rng$state(), state)
})


test_that("gamma random numbers prevent bad inputs", {
  r <- monty_rng$new(1)
  expect_equal(r$gamma_scale(1, 0, 0), 0)
  expect_equal(r$gamma_scale(1, Inf, Inf), Inf)

  expect_error(
    r$gamma_scale(1, -1.1, 5.1),
    "Invalid call to gamma with shape = -1.1, scale = 5.1")
  expect_error(
    r$gamma_scale(1, 5.1, -1.1),
    "Invalid call to gamma with shape = 5.1, scale = -1.1")
})


test_that("can generate negative binomial numbers", {
  m <- 1000000
  n <- 958
  p <- 0.004145
  yf <- monty_rng$new(1)$negative_binomial_prob(m, n, p)

  expect_equal(mean(yf), (1 - p) * n / p, tolerance = 1e-3)
  expect_equal(var(yf), ((1 - p) * n) / p^2, tolerance = 1e-2)
})


test_that("negative_binomial_mu follows from negative_binomial_prob", {
  rng1 <- monty_rng$new(seed = 1L)
  rng2 <- monty_rng$new(seed = 1L)
  prob <- 0.3
  size <- 20
  expect_identical(
    rng1$negative_binomial_mu(100, size, size * (1 - prob) / prob),
    rng2$negative_binomial_prob(100, size, prob))
})


test_that("deterministic negative binomial returns mean", {
  m <- 100
  p <- as.numeric(sample(10, m, replace = TRUE)) / 10
  n <- as.numeric(sample(10, m, replace = TRUE))

  rng <- monty_rng$new(1, deterministic = TRUE)
  expect_equal(rng$negative_binomial_prob(m, n, p), (1 - p) * n / p)
})


test_that("negative binomial prevents bad inputs", {
  expect_error(monty_rng$new(1)$negative_binomial_prob(1, 10, 0),
               "Invalid call to negative_binomial with size = 10, prob = 0")
  expect_error(monty_rng$new(1)$negative_binomial_prob(1, 0, 0.5),
               "Invalid call to negative_binomial with size = 0, prob = 0.5")
  expect_error(monty_rng$new(1)$negative_binomial_prob(1, 10, 1.5),
               "Invalid call to negative_binomial with size = 10, prob = 1.5")
  expect_error(monty_rng$new(1)$negative_binomial_prob(1, 10, Inf),
               "Invalid call to negative_binomial with size = 10, prob = inf")
  expect_error(monty_rng$new(1)$negative_binomial_prob(1, Inf, 0.4),
               "Invalid call to negative_binomial with size = inf, prob = 0.4")
})


test_that("can generate beta-binomial numbers", {
  m <- 1000000
  n <- 100
  a <- 1.5
  b <- 8.5

  yf <- monty_rng$new(1)$beta_binomial_ab(m, n, a, b)

  expect_equal(mean(yf), n * a / (a + b), tolerance = 1e-3)
  expect_equal(var(yf),
               n * a * b * (a + b + n) / ((a + b)^2 * (a + b + 1)),
               tolerance = 1e-2)
})


test_that("beta_binomial_prob follows from beta_binomial_ab", {
  rng1 <- monty_rng$new(seed = 1L)
  rng2 <- monty_rng$new(seed = 1L)
  size <- 20
  a <- 2
  b <- 5
  expect_identical(
    rng1$beta_binomial_prob(100, size, a / (a + b), 1 / (a + b + 1)),
    rng2$beta_binomial_ab(100, size, a, b))
})


test_that("deterministic beta-binomial returns mean", {
  m <- 100
  n <- as.numeric(sample(100, m, replace = TRUE))
  a <- runif(m, 0, 10)
  b <- runif(m, 0, 10)

  rng <- monty_rng$new(1, deterministic = TRUE)

  expect_equal(rng$beta_binomial_ab(m, n, a, b), n * a / (a + b))
})


test_that("beta-binomial prevents bad inputs", {
  expect_error(monty_rng$new(1)$beta_binomial_ab(1, -1, 2, 5),
               "Invalid call to beta_binomial with size = -1, a = 2, b = 5")
  expect_error(monty_rng$new(1)$beta_binomial_ab(1, 10, 0, 5),
               "Invalid call to beta_binomial with size = 10, a = 0, b = 5")
  expect_error(monty_rng$new(1)$beta_binomial_ab(1, 10, 2, 0),
               "Invalid call to beta_binomial with size = 10, a = 2, b = 0")
  expect_error(monty_rng$new(1)$beta_binomial_ab(1, Inf, 2, 5),
               "Invalid call to beta_binomial with size = inf, a = 2, b = 5")
  expect_error(monty_rng$new(1)$beta_binomial_ab(1, 10, Inf, 5),
               "Invalid call to beta_binomial with size = 10, a = inf, b = 5")
  expect_error(monty_rng$new(1)$beta_binomial_ab(1, 10, 2, Inf),
               "Invalid call to beta_binomial with size = 10, a = 2, b = inf")
})


test_that("can generate samples from the cauchy distribution", {
  ## This one is really hard to validate because the cauchy does not
  ## have any finite moments...
  n <- 100000
  ans <- monty_rng$new(2)$cauchy(n, 0, 1)
  expect_gt(ks.test(ans, "pcauchy")$p.value, 0.3)
  expect_equal(median(ans), 0, tolerance = 0.01)
})


test_that("deterministic cauchy throws", {
  rng <- monty_rng$new(1, deterministic = TRUE)
  expect_error(
    rng$cauchy(1, 0, 1),
    "Can't use Cauchy distribution deterministically; it has no mean")
})


test_that("regression tests of binomial issues", {
  rng <- monty_rng$new(1, seed = 42)
  ## all values 0..n represented:
  expect_setequal(rng$binomial(10000, 1, 0.5), 0:1)
  expect_setequal(rng$binomial(10000, 4, 0.2), 0:4)
  expect_setequal(rng$binomial(10000, 10, 0.5), 0:10)
})


test_that("can fetch rng state", {
  set.seed(1)
  expect_false(is.null(get_r_rng_state()))
  rm(list = ".Random.seed", envir = .GlobalEnv)
  expect_true(is.null(get_r_rng_state()))
})


test_that("exponential by mean agrees with rate", {
  ans1 <- monty_rng$new(1)$exponential_rate(100, 0.5)
  ans2 <- monty_rng$new(1)$exponential_mean(100, 2)
  expect_equal(ans1, ans2)
})


test_that("can sample from the beta distribution", {
  rng <- monty_rng$new(1, seed = 42)
  a <- 2.5
  b <- 1.5
  r <- rng$beta(1000000, a, b)
  expect_equal(mean(r), a / (a + b), tolerance = 1e-3)
  expect_equal(var(r), a * b / ((a + b)^2 * (a + b + 1)), tolerance = 1e-2)
})


test_that("beta generation algorithm is correct", {
  rng1 <- monty_rng$new(1, seed = 42)
  rng2 <- monty_rng$new(1, seed = 42)
  a <- 2.5
  b <- 1.5
  r <- rng1$beta(1, a, b)

  x <- rng2$gamma_scale(1, a, 1)
  y <- rng2$gamma_scale(1, b, 1)
  expect_equal(r, x / (x + y))
})


test_that("deterministic beta returns mean", {
  n <- 10
  a <- rexp(n)
  b <- rexp(n)

  rng <- monty_rng$new(1, deterministic = TRUE)
  state <- rng$state()

  expect_equal(rng$beta(n, a, b), a / (a + b))

  expect_equal(rng$state(), state)
})


test_that("reference implementation of truncated normal is reasonable", {
  min <- -1
  max <- 2
  res <- replicate(10, {
    cmp <- rnorm(10000)
    cmp <- cmp[cmp >= min & cmp <= max]
    res <- replicate(10000, truncated_normal_r(min, max))
    suppressWarnings(ks.test(res, cmp)$p.value)
  })
  expect_gt(sum(res > 0.05), 5)

  min <- -1
  max <- Inf
  replicate(10, {
    cmp <- rnorm(10000)
    cmp <- cmp[cmp >= min & cmp <= max]
    res <- replicate(10000, truncated_normal_r(min, max))
    suppressWarnings(ks.test(res, cmp)$p.value)
  })
  expect_gt(sum(res > 0.05), 5)
})
