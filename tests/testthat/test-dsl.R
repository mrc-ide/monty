test_that("Can run high level dsl function", {
  m <- mcstate_dsl("a ~ Normal(0, 1)")
  expect_s3_class(m, "mcstate_model")
  expect_equal(m$density(0), dnorm(0, 0, 1, log = TRUE))
})


test_that("can generate model with simple assignment", {
  x <- quote({
    mu <- 5
    a ~ Normal(mu, 1)
  })
  m <- mcstate_dsl(x)
  expect_s3_class(m, "mcstate_model")
  expect_equal(m$density(0), dnorm(0, 5, 1, log = TRUE))
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
