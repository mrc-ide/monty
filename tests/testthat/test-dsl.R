test_that("Can run high level dsl function", {
  expect_null(mcstate_dsl("a ~ Normal(0, 1)"))
  expect_null(mcstate_dsl(a ~ Normal(0, 1)))
  x <- quote(a ~ Normal(0, 1))
  expect_null(mcstate_dsl(x))
  expect_error(mcstate_dsl(y))
})
