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


test_that("multiple parameters through the ring are the same as single", {
  m <- monty_example("ring")
  xy <- rbind(runif(10), runif(10))
  expect_identical(m$density(xy), apply(xy, 2, m$density))
  expect_identical(m$gradient(xy), apply(xy, 2, m$gradient))
})


test_that("gradient calculation is correct at origin", {
  m <- monty_example("ring")
  expect_equal(m$gradient(c(0, 0)), c(0, 0))

  ## and for the multiple case:
  xy <- rbind(runif(10), runif(10))
  xy[, 4] <- c(0, 0)
  expect_identical(m$gradient(xy), apply(xy, 2, m$gradient))
})


test_that("gradient calculations agree with empirical estimate", {
  m <- monty_example("ring", r = 3, sd = 0.5)
  xy <- rbind(runif(10), runif(10))
  expect_equal(apply(xy, 2, function(p) numDeriv::grad(m$density, p)),
               m$gradient(xy))
})


test_that("can directly sample from ring", {
  ## Actually showing this has the correct distribution is not
  ## straightforward, though visually it's ok.  Noone should be
  ## relying on this anyway.
  r <- monty_rng_create(seed = 1, n_streams = 1)
  m <- monty_example("ring", r = 10, sd = 1)
  xy <- m$direct_sample(r)
  expect_length(xy, 2)
})


test_that("multiple parameters through the mixture are the same as single", {
  set.seed(1)
  m <- monty_example("mixture2d", spread = 5)
  xy <- rbind(runif(10), runif(10))
  expect_identical(m$density(xy), apply(xy, 2, m$density))
  expect_identical(m$gradient(xy), apply(xy, 2, m$gradient))
})


test_that("gradient calculations agree with empirical estimate", {
  set.seed(1)
  m <- monty_example("mixture2d", spread = 5)
  xy <- rbind(runif(10), runif(10))
  expect_equal(apply(xy, 2, function(p) numDeriv::grad(m$density, p)),
               m$gradient(xy))
})


test_that("can directly sample from mixture", {
  set.seed(1)
  ## Actually showing this has the correct distribution is not
  ## straightforward, though visually it's ok.  Noone should be
  ## relying on this anyway.
  r <- monty_rng_create(seed = 1, n_streams = 1)
  m <- monty_example("mixture2d", spread = 5)
  xy <- m$direct_sample(r)
  expect_length(xy, 2)
})


test_that("validate arguments to mixture2d", {
  expect_error(monty_example("mixture2d"), "spread' must be a scalar")
  expect_error(monty_example("mixture2d", spread = 1, means = 1),
               "Do not provide 'spread' if providing 'means'")
  expect_error(monty_example("mixture2d", means = 1),
               "Expected 'means' to be a matrix")
  expect_error(monty_example("mixture2d", means = matrix(0, 3, 3)),
               "Expected 'means' to have 2 columns")
  expect_error(monty_example("mixture2d", means = matrix(0, 3, 2)),
               "Expected 'means' to have 8 rows")
})
