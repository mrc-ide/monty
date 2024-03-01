test_that("can draw samples from a trivial model", {
  m <- ex_simple_gaussian(vcv = rbind(c(0.02, 0.01), c(0.01, 0.03)))
  sampler <- mcstate_sampler_adaptive(vcv = diag(c(0.01, 0.01)))
  res <- mcstate_sample(m, sampler, 5000)
  expect_equal(names(res), c("pars", "density", "details", "chain"))

  ## Initial vcv estimate was far too small; we've increased it.
  expect_gt(res$details[[1]]$vcv, 0.01)
})


test_that("If mean is zero, autocorrelation is the vcv", {
  vcv <- rbind(c(0.00057, 0.00034), c(0.00034, 0.00026))
  sampler <- mcstate_sampler_adaptive(vcv)
  state <- list(pars = c(0, 0))
  model <- NULL
  sampler$initialise(state, model)
  expect_equal(sampler$finalise(state, model)$autocorrelation, vcv)
})
