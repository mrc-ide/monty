test_that("can draw samples from a trivial model", {
  m <- ex_simple_gamma1()
  sampler <- mcstate_sampler_random_walk(vcv = matrix(0.01, 1, 1))
  res <- mcstate_sample(m, sampler, 100)
  expect_equal(names(res), c("pars", "density", "initial", "details"))
  expect_equal(dim(res$pars), c(1, 1, 100))
})


test_that("validate construction of rw sampler", {
  expect_error(
    mcstate_sampler_random_walk(),
    "One of 'proposal' or 'vcv' must be given")
  expect_error(
    mcstate_sampler_random_walk(identity, matrix(1, 1, 1)),
    "Only one of 'proposal' or 'vcv' may be given")
})


test_that("validate sampler against model on initialisation", {
  m <- ex_simple_gamma1()

  state <- list(pars = 1, density = -Inf)
  sampler1 <- mcstate_sampler_random_walk(vcv = diag(1) * 0.01)
  sampler2 <- mcstate_sampler_random_walk(vcv = diag(2) * 0.01)

  expect_no_error(sampler1$initialise(1, m))
  expect_error(
    sampler2$initialise(1, m),
    "Incompatible length parameters (1) and vcv (2)",
    fixed = TRUE)
})


test_that("can draw samples from a random model", {
  set.seed(1)
  m <- ex_dust_sir()
  vcv <- matrix(c(0.0006405, 0.0005628, 0.0005628, 0.0006641), 2, 2)
  sampler <- mcstate_sampler_random_walk(vcv = vcv)
  res <- mcstate_sample(m, sampler, 20)
  expect_setequal(names(res),
                  c("pars", "density", "initial", "details"))
})
