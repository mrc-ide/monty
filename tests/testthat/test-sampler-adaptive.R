test_that("can draw samples from a trivial model", {
  m <- local({
    shape <- 1
    rate <- 1
    mcstate_model(
      parameters = "gamma",
      sample = function() rgamma(1, shape = shape, rate = rate),
      density = function(x) dgamma(x, shape = shape, rate = rate, log = TRUE),
      gradient = function(x) (shape - 1) / x - shape,
      domain = rbind(c(0, Inf)))
  })
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
