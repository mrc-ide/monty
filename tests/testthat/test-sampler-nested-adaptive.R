test_that("can run a simple nested adaptive sampler", {
  set.seed(1)
  ng <- 3
  m <- ex_simple_nested(ng)
  v <- list(base = NULL,
            groups = rep(list(matrix(50)), ng))
  s <- mcstate_sampler_nested_adaptive(v)
  res <- mcstate_sample(m, s, 100)

  ## Something we can look for is that acceptances are not equally
  ## shared; I've used a very poor vcv here so that acceptance is
  ## relatively rare and we can see this:
  accept <- t(diff(t(res$pars[, , 1])) != 0)
  expect_setequal(colSums(accept), 0:3)
  expect_setequal(colSums(accept * 2^(seq_len(ng) - 1)), 0:7)
})


test_that("validate vcv inputs on construction of sampler", {
  expect_error(
    mcstate_sampler_nested_adaptive(NULL),
    "Expected a list for 'initial_vcv'")
  expect_error(
    mcstate_sampler_nested_adaptive(list()),
    "Expected 'initial_vcv' to have elements 'base' and 'groups'")
  expect_error(
    mcstate_sampler_nested_adaptive(list(base = TRUE, groups = TRUE)),
    "Expected 'initial_vcv$base' to be a matrix",
    fixed = TRUE)
  expect_error(
    mcstate_sampler_nested_adaptive(list(base = NULL, groups = TRUE)),
    "Expected 'initial_vcv$groups' to be a list",
    fixed = TRUE)
  expect_error(
    mcstate_sampler_nested_adaptive(list(base = NULL, groups = list())),
    "Expected 'initial_vcv$groups' to have at least one element",
    fixed = TRUE)
  expect_error(
    mcstate_sampler_nested_adaptive(list(base = NULL, groups = list(TRUE))),
    "Expected 'initial_vcv$groups[1]' to be a matrix",
    fixed = TRUE)

  vcv <- list(base = diag(1), groups = list(diag(2), diag(3)))
  expect_s3_class(mcstate_sampler_nested_adaptive(vcv), "mcstate_sampler")
})


test_that("can't use nested sampler with models that are not nested", {
  m <- ex_simple_gaussian(diag(3))
  vcv <- list(base = diag(1), groups = list(diag(2), diag(3)))
  s <- mcstate_sampler_nested_adaptive(vcv)
  expect_error(
    mcstate_sample(m, s, 100),
    "Your model does not have parameter groupings")
})


test_that("Validate that the model produces correct parameter groupings", {
  m <- mcstate_model(list(
    parameters = c("a", "b", "c"),
    density = function(x, by_group = TRUE) {
      x
    },
    parameter_groups = c(0, 1, 2)))
  s <- mcstate_sampler_nested_adaptive(
    list(base = diag(1), groups = list(diag(1), diag(1))))
  expect_error(
    mcstate_sample(m, s, 100, c(0, 0, 0)),
    "model$density(x, by_group = TRUE) did not produce a density",
    fixed = TRUE)
})


test_that("Validate length of by group output", {
  m <- mcstate_model(list(
    parameters = c("a", "b", "c"),
    density = function(x, by_group = TRUE) {
      structure(x, by_group = rep(0, 5))
    },
    parameter_groups = c(0, 1, 2)))
  s <- mcstate_sampler_nested_adaptive(
    list(base = diag(1), groups = list(diag(1), diag(1))))
  expect_error(
    mcstate_sample(m, s, 100, c(0, 0, 0)),
    "model$density(x, by_group = TRUE) produced a 'by_group' attribute with",
    fixed = TRUE)
})


test_that("can continue nested sampler correctly", {
  set.seed(1)
  ng <- 3
  m <- ex_simple_nested(ng)
  v <- list(base = NULL,
            groups = rep(list(matrix(1)), ng))
  sampler <- mcstate_sampler_nested_adaptive(v)

  set.seed(1)
  res1 <- mcstate_sample(m, sampler, 30, n_chains = 3, restartable = TRUE)

  set.seed(1)
  res2a <- mcstate_sample(m, sampler, 10, n_chains = 3, restartable = TRUE)
  res2b <- mcstate_sample_continue(res2a, 20, restartable = TRUE)

  expect_equal(res2b, res1)
})


test_that("can run a sampler with shared parameters", {
  set.seed(1)
  ng <- 5
  m <- ex_simple_nested_with_base(ng)
  s <- mcstate_sampler_nested_adaptive(
    list(base = diag(1), groups = rep(list(diag(1)), ng)))
  res <- mcstate_sample(m, s, 100)
  ## This is not great, until we get a real example we can use; but it
  ## does test that the sampler runs, and that it mixes.
  expect_true(length(unique(res$pars[1, , ])) > 1)
})


test_that("can run an observer during a nested fit", {
  set.seed(1)
  ng <- 5
  m <- ex_simple_nested_with_base(ng)
  s <- mcstate_sampler_nested_adaptive(
    list(base = diag(1), groups = rep(list(diag(1)), ng)))
  counter <- 0
  observer <- mcstate_observer(function(...) {
    counter <<- counter + 1
    list(n = counter)
  })
  res <- mcstate_sample(m, s, 100, observer = observer)
  expect_equal(
    dim(res$observations$n),
    c(1, 100, 1))
  expect_gt(max(res$observations$n), 120) # called way more than once per step
})
