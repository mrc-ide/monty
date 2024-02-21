test_that("repeated calls rmvnorm agrees with one-shot", {
  vcv <- matrix(c(4, 2, 2, 3), ncol = 2)
  x <- runif(4)
  g1 <- mcstate_rng$new(seed = 42)
  g2 <- mcstate_rng$new(seed = 42)
  r1 <- rmvnorm(x, vcv, g1)
  r2 <- make_rmvnorm(vcv)(x, g2)
  expect_identical(r2, r1)
})


test_that("rmvnorm has correct behaviour in trivial case", {
  vcv <- matrix(1, 1, 1)
  g1 <- mcstate_rng$new(seed = 42)
  g2 <- mcstate_rng$new(seed = 42)
  r1 <- replicate(100, rmvnorm(0, vcv, g1))
  r2 <- g2$random_normal(100)
  expect_identical(r1, r2)
})


## This is still just too slow to really push around:
test_that("rvnorm produces correct expectation", {
  vcv <- matrix(c(4, 2, 2, 3), ncol = 2)
  f <- make_rmvnorm(vcv)
  rng <- mcstate_rng$new(seed = 42)
  x <- c(1, 2)
  r <- t(replicate(1e5, f(x, rng)))
  expect_equal(colMeans(r), c(1, 2), tolerance = 0.01)
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
