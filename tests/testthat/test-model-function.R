test_that("can create model from function", {
  fn <- function(a, b) {
    dnorm(0, a, b)
  }
  m <- monty_model_function(fn)
  expect_s3_class(m, "monty_model")
  expect_equal(m$parameters, c("a", "b"))
  expect_equal(monty_model_density(m, c(1, 2)),
               dnorm(0, 1, 2))
})


test_that("density must be a function", {
  expect_error(monty_model_function(NULL),
               "Expected 'density' to be a function")
})


test_that("can provide a custom packer", {
  p <- monty_packer(c("a", "b"), fixed = list(x = 10))
  fn <- function(a, b, x) {
    dnorm(x, b, a)
  }
  m <- monty_model_function(fn, p)
  expect_equal(m$parameters, c("a", "b"))
  expect_equal(monty_model_density(m, c(1, 2)),
               dnorm(10, 2, 1))
})


test_that("packer must be a monty_packer if provided", {
  fn <- function(a, b) {
    dnorm(0, a, b)
  }
  expect_no_error(monty_model_function(fn, NULL))
  expect_error(
    monty_model_function(fn, TRUE),
    "Expected 'packer' to be a 'monty_packer' object")
})


test_that("can fix some data", {
  p <- monty_packer(c("a", "b"))
  fn <- function(a, b, x) {
    dnorm(x, b, a)
  }
  m <- monty_model_function(fn, fixed = list(x = 10))
  expect_equal(m$parameters, c("a", "b"))
  expect_equal(monty_model_density(m, c(1, 2)),
               dnorm(10, 2, 1))
  expect_error(
    monty_model_function(fn, p, fixed = list(x = 10)),
    "Can't provide both 'packer' and 'fixed'")
})


test_that("can compute vectorised densities", {
  fn <- function(x, y, s) {
    dnorm(x, y, s, log = TRUE)
  }
  m <- monty_model_function(fn, allow_multiple_parameters = TRUE)
  expect_true(m$properties$allow_multiple_parameters)
  expect_equal(m$density(c(0, 1, 2)), dnorm(0, 1, 2, log = TRUE))
  expect_equal(m$density(cbind(c(0, 1, 2))), dnorm(0, 1, 2, log = TRUE))
  expect_equal(m$density(cbind(c(0, 1, 2), c(3, 4, 5))),
               dnorm(c(0, 3), c(1, 4), c(2, 5), log = TRUE))
})


test_that("can cope with fixed parameters and multiple inputs", {
  fn <- function(a, b, c) {
    a + b + c
  }

  m <- monty_model_function(fn, fixed = list(c = 10),
                            allow_multiple_parameters = TRUE)
  expect_equal(m$parameters, c("a", "b"))
  expect_equal(m$density(c(1, 2)), 13)
  expect_equal(m$density(matrix(1:6, 2)), c(13, 17, 21))
})


test_that("can undo fixed arguments to packer", {
  fn <- function(a, b, c) {
    a + b + c
  }
  p <- monty_packer(c("a", "b"), fixed = list(c = 10))
  m <- monty_model_function(fn, packer = p,
                            allow_multiple_parameters = TRUE)
  expect_equal(m$parameters, c("a", "b"))
  expect_equal(m$density(c(1, 2)), 13)
  expect_equal(m$density(matrix(1:6, 2)), c(13, 17, 21))
})


test_that("can't use process in packer with multiple parameters", {
  fn <- function(a, b, c) {
    a + b + c
  }
  p <- monty_packer(c("a", "b"), process = function(x) list(c = 10))

  m <- monty_model_function(fn, packer = p)
  expect_equal(m$parameters, c("a", "b"))
  expect_equal(m$density(c(1, 2)), 13)

  expect_error(
    monty_model_function(fn, packer = p, allow_multiple_parameters = TRUE),
    "Can't use 'allow_multiple_parameters' with a packer")
})


test_that("can apply domain to model from function", {
  fn <- function(a, b) {
    dnorm(0, a, b, log = TRUE)
  }
  m <- monty_model_function(fn, domain = rbind(b = c(1, 5)))
  expect_s3_class(m, "monty_model")
  expect_equal(m$domain, rbind(a = c(-Inf, Inf), b = c(1, 5)))
  expect_equal(m$parameters, c("a", "b"))
  expect_equal(monty_model_density(m, c(1, 2)),
               dnorm(0, 1, 2, log = TRUE))
  expect_equal(monty_model_density(m, c(1, 6)), -Inf)
})


test_that("cannot use domain and multiple parameters", {
  fn <- function(a, b) {
    dnorm(0, a, b, log = TRUE)
  }
  domain <- rbind(b = c(1, 5))
  expect_error(
    monty_model_function(fn, domain = domain, allow_multiple_parameters = TRUE),
    "'allow_multiple_parameters' and 'domain' cannot yet be used together")
})


test_that("can expand domain", {
  fn <- function(a, b) {
    dpois(a, 1, log = TRUE) + sum(dnorm(b, log = TRUE))
  }
  packer <- monty_packer(scalar = "a", array = list("b" = 3))
  domain <- rbind(b = c(-4, 4))
  res <- monty_model_function(fn, packer = packer, domain = domain)
  expect_equal(
    res$domain,
    rbind(a = c(-Inf, Inf),
          "b[1]" = c(-4, 4),
          "b[2]" = c(-4, 4),
          "b[3]" = c(-4, 4)))
  expect_equal(res$density(c(0, 0, 0, 0)), -3.75681559961402)
  expect_equal(res$density(c(0, 9, 0, 0)), -Inf)
})
