test_that("can run hmc", {
  m <- ex_simple_gaussian(diag(2))
  sampler <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)
  set.seed(1)
  res <- mcstate_sample(m, sampler, 2000)
  ## We'll need some long tests here at some point!
  expect_equal(cov(unname(res$pars)), diag(2), tolerance = 0.2)
})

test_that("can run hmc on banana shaped model", {
  m <- ex_banana()
  sampler <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)
  set.seed(1)
  res <- mcstate_sample(m, sampler, 2000)
  ## We'll need some long tests here at some point!
  expect_equal(abs(mean(res$pars[,2])) < 0.2, TRUE)
  expect_equal(abs(mean(res$pars[,1]) - 1) < 0.4, TRUE)
  expect_equal(abs(cor(res$pars[,1],res$pars[,2])) < 0.3, TRUE)
})

test_that("can output debug traces", {
  m <- ex_simple_gaussian(diag(2))

  set.seed(1)
  sampler1 <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)
  res1 <- mcstate_sample(m, sampler1, 30)

  set.seed(1)
  sampler2 <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10,
                                  debug = TRUE)
  res2 <- mcstate_sample(m, sampler2, 30)

  expect_null(res1$details)
  expect_length(res2$details, 1)
  expect_equal(names(res2$details[[1]]), "debug")
  ## Marc: is this expected given things _should_ be fairly well
  ## behaved on this surface?
  expect_equal(res2$details[[1]]$debug$accept, rep(TRUE, 30))

  pars <- res2$details[[1]]$debug$pars
  expect_equal(dim(pars), c(11, 2, 30))
  expect_equal(dimnames(pars), list(NULL, c("a", "b"), NULL))
  expect_equal(t(pars[1, , ]), res2$pars[-31, ])
  expect_equal(t(pars[11, , ]), res2$pars[-1, ])
})


test_that("can use custom vcv", {
  m <- ex_simple_gaussian(diag(2))
  set.seed(1)
  sampler1 <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)
  res1 <- mcstate_sample(m, sampler1, 30)

  set.seed(1)
  sampler2 <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10,
                                  vcv = diag(2))
  res2 <- mcstate_sample(m, sampler2, 30)
  expect_identical(res2, res1)

  set.seed(1)
  sampler3 <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10,
                                  vcv = diag(2) * 0.5)
  res3 <- mcstate_sample(m, sampler3, 30)
  expect_false(identical(res3, res1))
})


test_that("given vcv and parameters must be compatible", {
  m <- ex_simple_gaussian(diag(2))
  set.seed(1)
  sampler <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10,
                                 vcv = diag(3))
  expect_error(
    mcstate_sample(m, sampler, 30),
    "Incompatible length parameters (2) and vcv (3)",
    fixed = TRUE)
})


test_that("can transform model parameters to R^n and back again", {
  transform <- hmc_transform(cbind(rep(-Inf, 3), rep(Inf, 3)))
  x <- c(0.1, 0.2, 0.3)
  expect_identical(transform$model2rn(x), x)
  expect_identical(transform$rn2model(x), x)
  expect_identical(transform$deriv(x), rep(1, 3))

  transform <- hmc_transform(cbind(rep(0, 3), rep(Inf, 3)))
  x <- c(0.5, 1.5, 2.5)
  expect_identical(transform$model2rn(x), log(x))
  expect_identical(transform$rn2model(log(x)), x)
  expect_identical(transform$deriv(x), x)

  lower <- c(0, 1, 2)
  upper <- c(1, 3, 6)
  transform <- hmc_transform(cbind(lower, upper))
  x <- c(0.5, 1.5, 2.5)
  expect_identical(transform$model2rn(x), logit_bounded(x, lower, upper))
  theta <- transform$model2rn(x)
  expect_equal(transform$rn2model(theta), x)
  expect_equal(transform$deriv(x), dilogit_bounded(x, lower, upper))

  ## A mixed case:
  transform <- hmc_transform(cbind(c(-Inf, 0, 0), c(Inf, Inf, 1)))
  x <- c(0.5, 0.6, 0.7)
  theta <- transform$model2rn(x)
  expect_equal(theta, c(0.5, log(0.6), logit_bounded(0.7, 0, 1)))
  expect_equal(transform$rn2model(theta), x)
  expect_equal(transform$deriv(x),
               c(1, 0.6, dilogit_bounded(0.7, 0, 1)))
})


test_that("prevent weird distributions", {
  expect_error(
    hmc_transform(cbind(rep(0, 3), c(1, Inf, -Inf))),
    "Unhandled domain type for parameter 3")
})
