test_that("Can run high level dsl function", {
  m <- mcstate_dsl("a ~ Normal(0, 1)")
  expect_s3_class(m, "mcstate_model")
  expect_equal(m$density(0), dnorm(0, 0, 1, log = TRUE))
  expect_equal(m$domain, rbind(a = c(-Inf, Inf)))
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
