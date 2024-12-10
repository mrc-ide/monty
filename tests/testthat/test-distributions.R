test_that("rmvnorm has correct behaviour in trivial case", {
  vcv <- matrix(1, 1, 1)
  g1 <- monty_rng_create(seed = 42)
  g2 <- monty_rng_create(seed = 42)
  mvn <- make_rmvnorm(vcv)
  r1 <- replicate(100, mvn(g1))
  r2 <- monty_random_n_normal(100, 0, 1, g2)
  expect_identical(r1, r2)
})


test_that("rvnorm produces correct expectation", {
  vcv <- matrix(c(4, 2, 2, 3), ncol = 2)
  f <- make_rmvnorm(vcv)
  rng <- monty_rng_create(seed = 42)
  r <- t(replicate(1e5, f(rng)))
  expect_equal(colMeans(r), c(0, 0), tolerance = 0.01)
  expect_equal(var(r), vcv, tolerance = 0.01)
})


test_that("can compute the log density of a multivariate normal", {
  skip_if_not_installed("mvtnorm")
  vcv <- matrix(c(4, 2, 2, 3), ncol = 2)
  set.seed(1)
  x <- matrix(rnorm(20, sd = 10), ncol = 2)
  expect_equal(apply(x, 1, make_ldmvnorm(vcv)),
               mvtnorm::dmvnorm(x, sigma = vcv, log = TRUE))
  expect_identical(apply(x, 1, make_ldmvnorm(vcv)),
                   apply(x, 1, ldmvnorm, vcv))
})


test_that("can compute derivatives of multivariate normal log density", {
  skip_if_not_installed("numDeriv")
  vcv <- matrix(c(4, 2, 2, 3), ncol = 2)
  set.seed(1)
  x <- matrix(rnorm(20, sd = 10), ncol = 2)
  f <- make_ldmvnorm(vcv)
  g <- make_deriv_ldmvnorm(vcv)
  expect_equal(apply(x, 1, g),
               apply(x, 1, function(xi) numDeriv::grad(f, xi)))
  expect_identical(apply(x, 1, g),
                   apply(x, 1, deriv_ldmvnorm, vcv))
})


test_that("Can draw samples from many single-variable MVNs", {
  r1 <- monty_rng_create(seed = 42, n_streams = 4)
  r2 <- monty_rng_create(seed = 42, n_streams = 4)
  vcv <- array(1, c(1, 1, 4))
  expect_equal(make_rmvnorm(vcv)(r2),
               monty_random_normal(0, 1, r1))
  expect_equal(make_rmvnorm(0.1 * vcv)(r2),
               monty_random_normal(0, 1, r1) * sqrt(0.1))
  expect_equal(make_rmvnorm(0.1 * 1:4 * vcv)(r2),
               monty_random_normal(0, 1, r1) * sqrt(0.1 * 1:4))
})


test_that("Can draw samples from many centred single-variable MVNs", {
  r1 <- monty_rng_create(seed = 42, n_streams = 4)
  r2 <- monty_rng_create(seed = 42, n_streams = 4)
  vcv <- array(1, c(1, 1, 4))
  x <- runif(4)
  expect_equal(make_rmvnorm(vcv)(r2),
               monty_random_normal(0, 1, r1))

  expect_equal(make_rmvnorm(vcv)(r2),
               monty_random_normal(0, 1, r1))
  expect_equal(make_rmvnorm(0.1 * vcv)(r2),
               monty_random_normal(0, 1, r1) * sqrt(0.1))
  expect_equal(make_rmvnorm(0.1 * 1:4 * vcv)(r2),
               monty_random_normal(0, 1, r1) * sqrt(0.1 * 1:4))
})


test_that("Can draw samples from many bivariate MVNs", {
  r1 <- monty_rng_create(seed = 42, n_streams = 5)
  r2 <- monty_rng_create(seed = 42, n_streams = 5)
  r3 <- monty_rng_create(seed = 42, n_streams = 5)
  vcv <- array(0, c(2, 2, 5))
  set.seed(1)
  vcv[1, 1, ] <- 1:5
  vcv[2, 2, ] <- 1
  vcv[1, 2, ] <- vcv[2, 1, ] <- rnorm(5, 0, 0.1)

  y <- make_rmvnorm(vcv)(r2)
  expect_equal(dim(y), c(2, 5))

  ## A bit of work do do these separately:
  rng_state <- matrix(monty_rng_state(r1), ncol = 5)
  z <- vapply(1:5, function(i) {
    r <- monty_rng_create(seed = rng_state[, i])
    make_rmvnorm(vcv[, , i])(r)
  }, numeric(2))
  expect_identical(y, z)
})
