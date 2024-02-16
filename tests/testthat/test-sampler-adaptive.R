test_that("can draw samples from a trivial model", {
  m <- ex_simple_gamma1()
  sampler <- mcstate_sampler_adaptive(vcv = matrix(0.01, 1, 1))
  res <- mcstate_sample(m, sampler, 5000)
  expect_equal(names(res), c("pars", "density", "details"))

  ## Initial vcv estimate was far too small; we've increased it.
  expect_gt(res$details$vcv, 0.01)
})


test_that("If mean is zero, autocorrelation is the vcv", {
  vcv <- rbind(c(0.00057, 0.00034), c(0.00034, 0.00026))
  sampler <- mcstate_sampler_adaptive(vcv)
  state <- list(pars = c(0, 0))
  model <- NULL
  sampler$initialise(state, model)
  expect_equal(sampler$finalise(state, model)$autocorrelation, vcv)
})
