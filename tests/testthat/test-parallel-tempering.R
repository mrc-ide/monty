test_that("can run a PT sampler", {
  likelihood <- ex_mixture(5)
  prior <- monty_dsl({
    x ~ Normal(0, 10)
  })
  posterior <- likelihood + prior
  s <- monty_sampler_parallel_tempering(n_rungs = 10, vcv = matrix(0.1))
  res <- monty_sample(posterior, s, 100, n_chains = 4)
  expect_type(res$details, "list")
  expect_equal(names(res$details[[1]]), "accept_swap")
  expect_length(res$details[[1]]$accept_swap, 10)
})
