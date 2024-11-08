test_that("can run a PT sampler", {
  likelihood <- ex_mixture(5)
  prior <- monty_dsl({
    x ~ Normal(0, 10)
  })
  posterior <- likelihood + prior
  s <- monty_parallel_tempering(n_rungs = 10, vcv = matrix(0.1))
  res <- monty_sample(posterior, s, 100, n_chains = 4)

  hist(c(res$pars), freq = FALSE)
  x <- seq(min(res$pars), max(res$pars), length.out = 1001)
  y <- exp(posterior$density(rbind(x)))
  lines(x, y / sum(y) / diff(x)[[1]], col = "red")
})
