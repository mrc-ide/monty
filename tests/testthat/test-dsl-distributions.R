test_that("check argument matching", {
  candidate <- alist(mean = , sd = )
  expect_equal(match_call_candidate(list("a", "b"), candidate), 1:2)
  expect_null(match_call_candidate(list("a", "b", "c"), candidate))
  expect_equal(match_call_candidate(list("a", sd = "b"), candidate), 1:2)
  expect_equal(match_call_candidate(list(mean = "a", sd = "b"), candidate), 1:2)
  expect_null(match_call_candidate(list(sd = "a", sd = "b"), candidate), 1:2)
  expect_null(match_call_candidate(list(mu = "a", sd = "b"), candidate), 1:2)
  expect_equal(match_call_candidate(list(sd = "a", mean = "b"), candidate), 2:1)
})


test_that("match against set of candidates", {
  candidates <- list(alist(a = , b = ),
                     alist(shape = , scale = ))
  expect_equal(match_call(list(0, 0), candidates),
               list(success = TRUE, index = 1, args = 1:2))
  expect_equal(match_call(list(0, b = 0), candidates),
               list(success = TRUE, index = 1, args = 1:2))
  expect_equal(match_call(list(a = 0, b = 0), candidates),
               list(success = TRUE, index = 1, args = 1:2))?
  expect_equal(match_call(list(b = 0, a = 0), candidates),
               list(success = TRUE, index = 1, args = 2:1))
  expect_equal(match_call(list(0, scale = 0), candidates),
               list(success = TRUE, index = 2, args = 1:2))
  expect_equal(match_call(list(scale = 0, shape = 0), candidates),
               list(success = TRUE, index = 2, args = 2:1))

  error <- c(x = "Failed to match given arguments: .",
             i = "Call should match one of:",
             ">" = "a, b",
             ">" = "scale, shape")
  expect_equal(
    match_call(list(0), candidates),
    list(success = FALSE, error = error))

  error <- c(x = "Failed to match given arguments: .",
             i = "Call should match:",
             ">" = "a, b")
  expect_equal(
    match_call(list(a = 1, b = 2, c = 3), candidates[1]),
    list(success = FALSE, error = error))
})


## These are all very poor tests in that they just restate the
## implementation.  I am not really clear how we can do better than
## this at the moment.  That said, there's a chance we'll swap out the
## implemnentation later for something symbolically differentiable so
## at that point these would become fairly useful.
test_that("can compute density for exponential distribution", {
  expect_equal(distr_exponential_rate$density(3.1, 0.4),
               dexp(3.1, 0.4, log = TRUE))
  expect_equal(distr_exponential_mean$density(3.1, 0.4),
               dexp(3.1, 1 / 0.4, log = TRUE))
})

test_that("can compute density for normal distribution", {
  expect_equal(distr_normal_rate$density(0, 1, 2),
               dnorm(0, 1, 2, log = TRUE)
})


test_that("can compute density for uniform distribution", {
  expect_equal(distr_normal_rate$uniform(1, 2, 3),
               dunif(1, 2, 3, log = TRUE))
})
