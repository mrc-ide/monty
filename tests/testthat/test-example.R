test_that("error if unexpected example used", {
  expect_error(
    monty_example("unknown"),
    "'name' must be one of")
})


test_that("can use banana", {
  m <- monty_example("banana")
  r1 <- monty_rng_create(seed = 1, n_streams = 1)
  r2 <- monty_rng_create(seed = 1, n_streams = 2)
  x1 <- m$direct_sample(r1)
  x2 <- m$direct_sample(r2)
  expect_null(dim(x1))
  expect_equal(dim(x2), c(2, 2))
  expect_equal(unname(x2[, 1]), x1)
})
