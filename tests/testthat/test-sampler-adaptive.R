test_that("Empirical VCV calculated correctly", {
  
  m <- ex_simple_gaussian(vcv = rbind(c(0.02, 0.01), c(0.01, 0.03)))
  
  sampler <- mcstate_sampler_adaptive(initial_vcv = diag(c(0.01, 0.01)),
                                      forget_rate = 0)
  res <- mcstate_sample(m, sampler, 5000)
  expect_equal(names(res), c("pars", "density", "details", "chain"))
  ## forget_rate = 0 so full chain should be included in VCV
  expect_equal(res$details[[1]]$weight, 5000)
  expect_equal(res$details[[1]]$included, seq_len(5000))
  expect_equal(res$details[[1]]$vcv, cov(res$pars[2:5001,]), ignore_attr = TRUE)
  
  
  sampler <- mcstate_sampler_adaptive(initial_vcv = diag(c(0.01, 0.01)),
                                      forget_rate = 0.1)
  res <- mcstate_sample(m, sampler, 5000)
  ## forget_rate = 0.1 so VCV should exclude first 500 parameter sets
  expect_equal(res$details[[1]]$weight, 4500)
  expect_equal(res$details[[1]]$included, seq(501, 5000, by = 1))
  expect_equal(res$details[[1]]$vcv, cov(res$pars[502:5001,]),
               ignore_attr = TRUE)
  
  
  sampler <- mcstate_sampler_adaptive(initial_vcv = diag(c(0.01, 0.01)),
                                      forget_rate = 0.5,
                                      forget_end = 500)
  res <- mcstate_sample(m, sampler, 5000)
  ## forget_rate = 0.5 and forget_end = 500 so VCV should exclude first
  ## 250 parameter sets
  expect_equal(res$details[[1]]$weight, 4750)
  expect_equal(res$details[[1]]$included, seq(251, 5000, by = 1))
  expect_equal(res$details[[1]]$vcv, cov(res$pars[252:5001,]),
               ignore_attr = TRUE)
  
})
