test_that("can compute density of a model", {
  m <- monty_model(
    list(
      parameters = c("a", "b"),
      density = function(x) {
        a <- x[[1]]
        b <- x[[2]]
        dnorm(a, 0, 1, log = TRUE) + dnorm(b, 0, 10, log = TRUE)
      }))
  expect_equal(monty_model_density(m, c(1, 2)),
               m$density(c(1, 2)))
  expect_error(
    monty_model_density(m, cbind(c(1, 2))),
    "'parameters' cannot be a matrix")
  expect_error(
    monty_model_density(m, c(1, 2, 3)),
    "Expected 'parameters' to have length 2, but it had length 3")
  expect_error(
    monty_model_density(m, c(b = 1, a = 2)),
    "'parameters' has names but these disagree with 'model$parameters'",
    fixed = TRUE)
})


test_that("can compute multiple densities if enabled", {
  m <- monty_model(
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
    properties = monty_model_properties(allow_multiple_parameters = TRUE))

  expect_equal(monty_model_density(m, c(1, 2)),
               m$density(c(1, 2)))
  expect_equal(monty_model_density(m, rbind(1, 2)),
               m$density(rbind(1, 2)))
  expect_equal(monty_model_density(m, rbind(1:2, 2:3)),
               m$density(rbind(1:2, 2:3)))
  expect_error(monty_model_density(m, rbind(1, 2, 3)),
               "Expected 'parameters' to have 2 rows, but it had 3 rows")
  expect_error(
    monty_model_density(m, rbind(b = 1, a = 2)),
    "'parameters' has rownames but these disagree with 'model$parameters'",
    fixed = TRUE)
})


test_that("can compute gradient where enabled", {
  m <- ex_simple_gamma1()
  expect_equal(monty_model_gradient(m, 1), -1)
  expect_equal(monty_model_gradient(m, 1, named = TRUE), c(gamma = -1))
})


test_that("can directly sample from a model", {
  m <- ex_simple_gamma1()
  r1 <- monty_rng_create(seed = 42)
  r2 <- monty_rng_create(seed = 42)

  expect_equal(monty_model_direct_sample(m, r1),
               m$direct_sample(r2))
  expect_equal(monty_model_direct_sample(m, r1, named = TRUE),
               c(gamma = m$direct_sample(r2)))
})


test_that("can compute multiple gradients", {
  m <- monty_example("banana")
  expect_equal(monty_model_gradient(m, c(0, 0)), c(0, 0))
  expect_equal(monty_model_gradient(m, c(0, 0), named = TRUE),
               c(alpha = 0, beta = 0))

  p <- cbind(c(0, 0), c(1, 0), c(0, 1))
  expect_equal(monty_model_gradient(m, p),
               rbind(c(0, -4, 4), c(0, 0, -9)))
  expect_equal(monty_model_gradient(m, p, named = TRUE),
               rbind(alpha = c(0, -4, 4), beta = c(0, 0, -9)))
})
