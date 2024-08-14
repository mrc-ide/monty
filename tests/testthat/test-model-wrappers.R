test_that("can compute density of a model", {
  m <- mcstate_model(
    list(
      parameters = c("a", "b"),
      density = function(x) {
        a <- x[[1]]
        b <- x[[2]]
        dnorm(a, 0, 1, log = TRUE) + dnorm(b, 0, 10, log = TRUE)
      }))
  expect_equal(mcstate_model_density(m, c(1, 2)),
               m$density(c(1, 2)))
  expect_error(
    mcstate_model_density(m, cbind(c(1, 2))),
    "'parameters' cannot be a matrix")
  expect_error(
    mcstate_model_density(m, c(1, 2, 3)),
    "Expected 'parameters' to have length 2, but it had length 3")
  expect_error(
    mcstate_model_density(m, c(b = 1, a = 2)),
    "'parameters' has names but these disagree with 'model$parameters'",
    fixed = TRUE)
})


test_that("can compute multiple densities if enabled", {
  m <- mcstate_model(
    list(
      parameters = c("a", "b"),
      density = function(x) {
        if (is.matrix(x)) {
          a <- x[1, ]
          b <- x[2, ]
        } else {
          a <- x[[1]]
          b <- x[[2]]
        }
        dnorm(a, 0, 1, log = TRUE) + dnorm(b, 0, 10, log = TRUE)
      }),
    properties = mcstate_model_properties(allow_multiple_parameters = TRUE))

  expect_equal(mcstate_model_density(m, c(1, 2)),
               m$density(c(1, 2)))
  expect_equal(mcstate_model_density(m, rbind(1, 2)),
               m$density(rbind(1, 2)))
  expect_equal(mcstate_model_density(m, rbind(1:2, 2:3)),
               m$density(rbind(1:2, 2:3)))
  expect_error(mcstate_model_density(m, rbind(1, 2, 3)),
               "Expected 'parameters' to have 2 rows, but it had 3 rows")
  expect_error(
    mcstate_model_density(m, rbind(b = 1, a = 2)),
    "'parameters' has rownames but these disagree with 'model$parameters'",
    fixed = TRUE)
})
