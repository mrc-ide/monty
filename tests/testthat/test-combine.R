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


test_that("direct sampling must be for a superset of parameters", {
  a <- mcstate_model(list(parameters = "y",
                          density = function(x) dnorm(x, log = TRUE)))
  b <- mcstate_model(list(
    parameters = c("x", "y"),
    density = function(x) sum(dnorm(x, sd = c(1, 10), log = TRUE)),
    direct_sample = function(rng) rng$normal(2, 0, c(1, 10))))
  ab <- a + b
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
