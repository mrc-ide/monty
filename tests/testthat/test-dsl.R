test_that("Can run high level dsl function", {
  m <- mcstate_dsl("a ~ Normal(0, 1)")
  expect_s3_class(m, "mcstate_model")
  expect_equal(m$density(0), dnorm(0, 0, 1, log = TRUE))
  expect_equal(m$domain, rbind(a = c(-Inf, Inf)))
  expect_equal(m$gradient(0), 0)
})


test_that("can generate model with simple assignment", {
  x <- quote({
    mu <- 5
    a ~ Normal(mu, 1)
  })
  m <- mcstate_dsl(x)
  expect_s3_class(m, "mcstate_model")
  expect_equal(m$density(0), dnorm(0, 5, 1, log = TRUE))
  expect_equal(m$domain, rbind(a = c(-Inf, Inf)))
  expect_equal(m$gradient(0), 5)
  expect_equal(m$gradient(2), 3)
})


test_that("can sample from a simple model", {
  m <- mcstate_dsl("a ~ Normal(0, 1)")
  expect_s3_class(m, "mcstate_model")
  expect_true(m$properties$has_direct_sample)

  r <- mcstate_rng$new(seed = 42)
  cmp <- r$normal(1, 0, 1)

  r <- mcstate_rng$new(seed = 42)
  expect_equal(m$direct_sample(r), cmp)
})


test_that("can sample from a model with assignments", {
  m <- mcstate_dsl({
    mu <- 5
    a ~ Normal(mu, 1)
  })
  r <- mcstate_rng$new(seed = 42)
  cmp <- r$normal(1, 5, 1)

  r <- mcstate_rng$new(seed = 42)
  expect_equal(m$direct_sample(r), cmp)
})


test_that("Can compute domain for uniform distribution variables", {
  m <- mcstate_dsl({
    a <- 1
    x ~ Uniform(a, 2)
  })
  expect_equal(m$domain, rbind(x = c(1, 2)))
})


test_that("Can evaluate through chains of assignments to compute domain", {
  m <- mcstate_dsl({
    a <- 1
    b <- a * 2
    c <- b + a
    x ~ Uniform(a, c)
  })
  expect_equal(m$domain, rbind(x = c(1, 3)))
})


test_that("give up on bounds if they come from a stochastic process", {
  m <- mcstate_dsl({
    a <- 5
    b ~ Normal(a, 1)
    c ~ Uniform(a, b)
    d ~ Uniform(b, 10)
    e ~ Uniform(c, d)
  })
  expect_equal(m$domain,
               rbind(b = c(-Inf, Inf),
                     c = c(5, Inf),
                     d = c(-Inf, 10),
                     e = c(-Inf, Inf)))
})


test_that("can prevent creation of gradient function", {
  code <- "a ~ Normal(0, 1)"
  m1 <- mcstate_dsl(code, gradient = FALSE)
  expect_false(m1$properties$has_gradient)
  expect_null(m1$gradient)

  m2 <- mcstate_dsl(code, gradient = TRUE)
  expect_true(m2$properties$has_gradient)
  expect_true(is.function(m2$gradient))

  m3 <- mcstate_dsl(code, gradient = NULL)
  expect_true(m3$properties$has_gradient)
  expect_true(is.function(m3$gradient))
})


test_that("handle failure to create gradient function", {
  code <- "a ~ Normal(0, 1)\nb ~ Normal(trigamma(a), 1)"
  expect_no_warning(m1 <- mcstate_dsl(code, gradient = FALSE))
  expect_false(m1$properties$has_gradient)
  expect_null(m1$gradient)

  err <- expect_error(
    mcstate_dsl(code, gradient = TRUE),
    "Failed to differentiate this model")
  expect_s3_class(err$parent, "mcstate_differentiation_failure")

  w <- expect_warning(
    m3 <- mcstate_dsl(code, gradient = NULL),
    "Not creating a gradient function for this model")
  expect_s3_class(w$parent, "mcstate2_parse_error")
  expect_s3_class(w$parent$parent, "mcstate_differentiation_failure")
  expect_false(m3$properties$has_gradient)
  expect_null(m3$gradient)
})


test_that("can compute gradients of complicated models", {
  m <- mcstate_dsl({
    a ~ Normal(0, 1)
    x <- a^2
    b ~ Exponential(2)
    c ~ Normal(x, b)
  })

  expect_equal(m$parameters, c("a", "b", "c"))
  expect_true(m$properties$has_gradient)

  p <- c(-.8, 0.03, 0.7)

  ## Silly density:
  expect_equal(
    m$density(p),
    dnorm(p[[1]], 0, 1, log = TRUE) +
    dexp(p[[2]], 2, log = TRUE) +
    dnorm(p[[3]], p[[1]]^2, p[[2]], log = TRUE))

  expect_equal(m$gradient(p), numDeriv::grad(m$density, p))
})
