test_that("can print samples", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  s <- monty_sample(model, sampler, 100, 1, n_chains = 3)

  res <- evaluate_promise(withVisible(print(s)))
  expect_mapequal(res$result, list(value = s, visible = FALSE))
  expect_match(
    res$messages,
    "<monty_samples: 1 parameter x 100 samples x 3 chains>",
    fixed = TRUE, all = FALSE)
})


test_that("print information about observations", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  s <- monty_sample(model, sampler, 100, 1, n_chains = 3)

  res <- evaluate_promise(print(s))
  expect_no_match(
    res$messages,
    "These samples have associated observations",
    fixed = TRUE, all = FALSE)

  s$observations <- list(a = 1)
  res <- evaluate_promise(print(s))
  expect_match(
    res$messages,
    "These samples have associated observations",
    fixed = TRUE, all = FALSE)
})


test_that("print information about restart", {
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  s <- monty_sample(model, sampler, 100, 1, n_chains = 3)

  res <- evaluate_promise(print(s))
  expect_no_match(
    res$messages,
    "These samples can be restared with",
    fixed = TRUE, all = FALSE)

  s$restart <- list()
  res <- evaluate_promise(print(s))
  expect_match(
    res$messages,
    "These samples can be restared with",
    fixed = TRUE, all = FALSE)
})


test_that("can convert to posterior draws types", {
  skip_if_not_installed("posterior")
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  s <- monty_sample(model, sampler, 100, 1, n_chains = 3)

  s_df <- posterior::as_draws_df(s)
  expect_s3_class(s_df, "draws_df")
  expect_setequal(names(s_df), c("gamma", ".chain", ".iteration", ".draw"))

  s_arr <- posterior::as_draws_array(s)
  expect_s3_class(s_arr, "draws_array")
  expect_equal(dimnames(s_arr),
               list(chain = NULL, iteration = NULL, variable = "gamma"))
})


test_that("can convert to coda type", {
  skip_if_not_installed("coda")
  model <- ex_simple_gamma1()
  sampler <- monty_sampler_random_walk(vcv = diag(1) * 0.01)
  s <- monty_sample(model, sampler, 100, 1, n_chains = 3)

  s_coda <- coda::as.mcmc.list(s)
  expect_s3_class(s_coda, "mcmc.list")
  expect_length(s_coda, 3)
  expect_s3_class(s_coda[[1]], "mcmc")
})
