test_that("can combine two simple models", {
  a <- mcstate_model(list(parameters = "x",
                          density = function(x) dnorm(x, log = TRUE)))
  b <- mcstate_model(list(parameters = "x",
                          density = function(x) dexp(x, log = TRUE)))
  ab <- a + b
  expect_equal(ab$properties, a$properties)
  expect_equal(ab$density(2), a$density(2) + b$density(2))
})


test_that("can combine a model with direct_sample and one without", {
  a <- mcstate_model(list(
    parameters = "x",
    density = function(x) dnorm(x, log = TRUE),
    direct_sample = function(rng) rng$random_normal(1)))
  b <- mcstate_model(list(
    parameters = "x",
    density = function(x) dexp(x, log = TRUE)))
  ab <- a + b
  expect_equal(ab$properties, a$properties)

  r1 <- mcstate_rng$new(seed = 42)
  r2 <- mcstate_rng$new(seed = 42)
  expect_equal(ab$direct_sample(r1), a$direct_sample(r2))
})


test_that("can't create direct_sample if both models have method", {
  a <- ex_simple_gamma1()
  b <- ex_simple_gamma1()
  ab <- a + b
  expect_null(ab$direct_sample)
  expect_false(ab$properties$has_direct_sample)
  properties <- mcstate_model_properties(has_direct_sample = TRUE)
  expect_error(
    mcstate_model_combine(a, b, properties = properties),
    "Can't create a direct_sample from these models")
})


test_that("can't create direct_sample if neither model has method", {
  a <- mcstate_model(list(parameters = "x",
                          density = function(x) dnorm(x, log = TRUE)))
  properties <- mcstate_model_properties(has_direct_sample = TRUE)
  expect_error(
    mcstate_model_combine(a, a, properties = properties),
    "Can't create a direct_sample from these models")
})


test_that("can combine domains", {
  a <- mcstate_model(list(parameters = "y",
                          density = identity,
                          domain = rbind(c(-5, 5))))
  b <- mcstate_model(list(parameters = c("x", "y"),
                          density = identity,
                          domain = rbind(c(-10, 10),
                                         c(-3, 10))))
  ab <- a + b
  expect_equal(ab$domain,
               rbind(y = c(-3, 5), x = c(-10, 10)))
})


test_that("direct sampling may reorder parameters", {
  a <- mcstate_model(list(parameters = "y",
                          density = function(x) dnorm(x, log = TRUE)))
  b <- mcstate_model(list(
    parameters = c("x", "y"),
    density = function(x) sum(dnorm(x, sd = c(1, 10), log = TRUE)),
    direct_sample = function(rng) rng$normal(2, 0, c(1, 10))))
  ab <- a + b
  expect_true(ab$properties$has_direct_sample)
  r1 <- mcstate_rng$new(seed = 1)
  r2 <- mcstate_rng$new(seed = 1)
  expect_equal(ab$direct_sample(r1),
               b$direct_sample(r2)[2:1]) # reversed, to align parameters
})


test_that("direct sampling requires that only one model is sampleable", {
  a <- ex_simple_gamma1(1)
  b <- ex_simple_gamma1(2)
  err <- expect_error(
    mcstate_model_combine(a, b,
                          mcstate_model_properties(has_direct_sample = TRUE)),
    "Can't create a direct_sample from these models")
  expect_match(err$body, "Both models have a 'direct_sample'")
  ab <- mcstate_model_combine(
    a, b, mcstate_model_properties(has_direct_sample = FALSE))
  expect_false(ab$properties$has_direct_sample)
})


test_that("direct sampling requires that a model is sampleable", {
  a <- mcstate_model(list(parameters = "a", density = identity))
  err <- expect_error(
    mcstate_model_combine(a, a,
                          mcstate_model_properties(has_direct_sample = TRUE)),
    "Can't create a direct_sample from these models")
  expect_match(err$body, "Neither of your models have 'direct_sample' methods")
})


test_that("require that all parameters can be sampled", {
  x <- mcstate_model(list(
    parameters = c("a", "b"),
    density = identity))
  y <- mcstate_model(list(
    parameters = c("a", "b", "c"),
    density = identity,
    direct_sample = identity))
  z <- mcstate_model(list(
    parameters = c("a", "b", "c", "d"),
    density = identity))
  expect_true((x + y)$properties$has_direct_sample)
  expect_false((x + z)$properties$has_direct_sample)
  expect_error(
    y + z,
    "Can't create a direct_sample from these models as 'a' does not")
})


test_that("validate input args", {
  x <- ex_simple_gamma1()
  expect_error(
    mcstate_model_combine(x, NULL),
    "Expected 'b' to be a 'mcstate_model' object")
  expect_error(
    mcstate_model_combine(NULL, x),
    "Expected 'a' to be a 'mcstate_model' object")
  expect_error(
    mcstate_model_combine(x, x, TRUE),
    "Expected 'properties' to be a 'mcstate_model_properties' object")
  expect_error(
    x + NULL,
    "Addition via '+' is only defined for 'mcstate_model",
    fixed = TRUE)
})


test_that("can combine models with gradients", {
  a <- ex_simple_gamma1(1)
  b <- ex_simple_gamma1(2)
  ab <- a + b
  expect_true(ab$has_gradient)
  expect_equal(ab$gradient(3),
               a$gradient(3) + b$gradient(3))
})


test_that("both models require gradients to create gradient function", {
  a <- ex_simple_gamma1(1)
  b <- mcstate_model(list(parameters = "x",
                          density = function(x) dnorm(x, log = TRUE)))
  expect_false(
    mcstate_model_combine(a, b)$properties$has_gradient)
  p <- mcstate_model_properties(has_gradient = FALSE)
  expect_false(
    mcstate_model_combine(a, b, p)$properties$has_gradient)
  p <- mcstate_model_properties(has_gradient = TRUE)
  expect_error(
    mcstate_model_combine(a, b, p),
    "Can't create a gradient from these models")
})


test_that("can combine gradients where parameters do not agree", {
  a <- mcstate_model(list(
    parameters = c("y", "z"),
    density = identity,
    gradient = sqrt))
  b <- mcstate_model(list(
    parameters = c("x", "y"),
    density = identity,
    gradient = sqrt))
  ab <- a + b
  expect_true(ab$properties$has_gradient)
  skip("wip")
  expect_equal(
    ab$gradient(c(2, 3, 4)),
    c(sqrt(2), sqrt(3) + log(3), log(3)))
})
