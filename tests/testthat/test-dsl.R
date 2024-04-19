test_that("Can run high level dsl function", {
  m <- mcstate_dsl("a ~ Normal(0, 1)")
  expect_s3_class(m, "mcstate_model")
  expect_equal(m$density(0), dnorm(0, 0, 1, log = TRUE))
})


test_that("can generate model with simple assignement", {
  x <- quote({
    mu <- 5
    a ~ Normal(mu, 1)
  })
  m <- mcstate_dsl(x)
  expect_s3_class(m, "mcstate_model")
  expect_equal(m$density(0), dnorm(0, 5, 1, log = TRUE))
})
