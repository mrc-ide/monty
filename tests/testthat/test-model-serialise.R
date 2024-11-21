test_that("can repair a model", {
  m <- ex_stochastic()
  ll <- m$density(0)

  m2 <- unserialize(suppressWarnings(serialize(m, NULL)))
  expect_error(
    m2$density(0),
    "Pointer has been serialised")

  m2$restore()
  expect_no_error(m2$density(0))
})


test_that("can repair a combined model", {
  m <- ex_stochastic()
  p <- monty_dsl({
    x ~ Normal(0, 1)
  })

  m2 <- m + p
  m2$density(0)

  m3 <- unserialize(suppressWarnings(serialize(m2, NULL)))
  expect_error(
    m3$density(0),
    "Pointer has been serialised")
  m3$restore()
  expect_no_error(m3$density(0))
})
