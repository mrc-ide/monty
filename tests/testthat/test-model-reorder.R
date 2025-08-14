test_that("can reorder a monty model", {
  ## Here's our source of truth, this is at least nice and easy:
  f <- monty_dsl({
    a ~ Exponential(mean = 1)
    b ~ Exponential(mean = 5)
    c ~ Exponential(mean = 25)
    d ~ Exponential(mean = 125)
  })

  g <- monty_dsl({
    c ~ Exponential(mean = 25)
    b ~ Exponential(mean = 5)
    d ~ Exponential(mean = 125)
    a ~ Exponential(mean = 1)
  })
  h <- monty_model_reorder(g, f$parameters)

  expect_equal(h$parameters, f$parameters)
  expect_equal(h$domain, f$domain)

  set.seed(1)
  x <- runif(4, max = 10)
  expect_identical(h$density(x), f$density(x))
  expect_identical(h$gradient(x), f$gradient(x))

  xx <- matrix(runif(4 * 3, max = 10), 4, 3)
  expect_identical(h$density(xx), f$density(xx))
  expect_identical(h$gradient(xx), f$gradient(xx))

  ## Testing direct sample is much harder because the order of samples
  ## now varies.  But these numbers are chosen to have sufficiently
  ## different means that we can just test ordering:
  r <- direct_sample_many(100, h, monty_rng_create(seed = 1))
  expect_true(all(rowMeans(r) > 0))
})


test_that("reordering requires that parameters are the same", {
  f <- monty_dsl({
    a ~ Normal(0, 1)
    b ~ Normal(0, 1)
  })

  expect_error(
    monty_model_reorder(f, c("a", "b", "c")),
    "Can't reorder 'model' as 'parameters' contains different values")
  expect_error(
    monty_model_reorder(f, "a"),
    "Can't reorder 'model' as 'parameters' contains different values")
})


test_that("don't create gradient function if not present", {
  f <- monty_model_function(function(a, b) 1)
  g <- monty_model_reorder(f, c("b", "a"))
  expect_equal(f$properties, g$properties)
  expect_null(f$gradient)
  expect_null(f$direct_sample)
})
