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
