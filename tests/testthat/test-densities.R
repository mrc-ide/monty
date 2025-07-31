## This one is the easiest:
test_that("density::poisson agrees", {
  lambda <- rexp(50)
  x <- as.integer(runif(length(lambda), 0, 50))
  expect_equal(dpois(x, lambda, TRUE),
               density_poisson(x, lambda, TRUE))
  expect_equal(dpois(x, lambda, FALSE),
               density_poisson(x, lambda, FALSE))
  
  ## Corner cases
  expect_equal(density_poisson(0L, 0, TRUE), dpois(0, 0, TRUE))
  expect_equal(density_poisson(0L, 0, FALSE), dpois(0, 0, FALSE))
  expect_equal(dpois(1L, 0, TRUE), density_poisson(1L, 0, TRUE))
  
  ## Outside domain
  expect_identical(density_poisson(-1L, 10, FALSE), 0)
  expect_identical(density_poisson(-1L, 10, TRUE), -Inf)
})


test_that("density::normal agrees", {
  mu <- runif(50, -100, 100)
  x <- rnorm(length(mu), mu, sd = runif(length(mu), max = 100))
  sd <- runif(length(x), max = 100)
  expect_equal(dnorm(x, mu, sd, TRUE),
               density_normal(x, mu, sd, TRUE))
  expect_equal(dnorm(x, mu, sd, FALSE),
               density_normal(x, mu, sd, FALSE))
  
  ## Corner cases
  expect_equal(density_normal(1, 1, 0, TRUE), dnorm(1, 1, 0, TRUE))
  expect_equal(density_normal(1, 1, 0, FALSE), dnorm(1, 1, 0, FALSE))
  expect_equal(density_normal(0, 1, 0, TRUE), dnorm(0, 1, 0, TRUE))
  expect_equal(density_normal(0, 1, 0, FALSE), dnorm(0, 1, 0, FALSE))
})


test_that("density::binomial agrees", {
  size <- as.integer(0:50)
  prob <- runif(length(size))
  x <- as.integer(runif(length(size), 0, size))
  expect_equal(density_binomial(x, size, prob, TRUE),
               dbinom(x, size, prob, TRUE))
  expect_equal(density_binomial(x, size, prob, FALSE),
               dbinom(x, size, prob, FALSE))
  
  ## Corner cases
  expect_equal(density_binomial(0L, 0L, 0, TRUE), dbinom(0, 0, 0, TRUE))
  expect_equal(density_binomial(0L, 0L, 0, FALSE), dbinom(0, 0, 0, FALSE))
  expect_equal(density_binomial(0L, 0L, 0.5, TRUE), dbinom(0, 0, 0.5, TRUE))
  expect_equal(density_binomial(10L, 0L, 0, TRUE), dbinom(10L, 0L, 0, TRUE))
  expect_equal(density_binomial(10L, 4L, 0.5, TRUE), dbinom(10L, 4L, 0.5, TRUE))
  
  ## Outside domain
  expect_identical(density_binomial(-1L, 10L, 0.5, FALSE), 0)
  expect_identical(density_binomial(-1L, 10L, 0.5, TRUE), -Inf)
})


test_that("density::negative_binomial agrees", {
  set.seed(1)
  for (is_float in c(FALSE, TRUE)) {
    if (is_float) {
      tolerance <- sqrt(sqrt(.Machine$double.eps))
    } else {
      tolerance <- sqrt(.Machine$double.eps)
    }
    
    size <- as.numeric(1:50)
    prob <- runif(length(size))
    mu <- size * (1 - prob) / prob
    x <- as.integer(sample(size, replace = TRUE))
    expect_equal(density_negative_binomial_mu(x, size, mu, TRUE, is_float),
                 dnbinom(x, size, mu = mu, log = TRUE),
                 tolerance = tolerance)
    expect_equal(density_negative_binomial_mu(x, size, mu, FALSE, is_float),
                 dnbinom(x, size, mu = mu, log = FALSE),
                 tolerance = tolerance)
    
    ## size > x case
    expect_equal(
      dnbinom(511, 2, mu = 6.65, log = TRUE),
      density_negative_binomial_mu(511L, 2, 6.65, TRUE, is_float),
      tolerance = tolerance)
    
    ## Allow non integer size
    expect_equal(
      density_negative_binomial_mu(511L, 3.5, 1, TRUE, is_float),
      dnbinom(511, 3.5, mu = 1, log = TRUE),
      tolerance = tolerance)
    
    ## Corner cases
    expect_equal(density_negative_binomial_mu(0L, 0, 0, TRUE, is_float),
                 dnbinom(0, 0, mu = 0, log = TRUE))
    expect_equal(density_negative_binomial_mu(0L, 0, 0, FALSE, is_float),
                 dnbinom(0, 0, mu = 0, log = FALSE))
    expect_equal(density_negative_binomial_mu(0L, 0, 0.5, FALSE, is_float),
                 dnbinom(0, 0, mu = 0.5, log = FALSE))
    expect_equal(density_negative_binomial_mu(10L, 0, 1, TRUE, is_float),
                 suppressWarnings(dnbinom(10L, 0L, mu = 1, log = TRUE)))
    expect_equal(density_negative_binomial_mu(10L, 1, 1, TRUE, is_float),
                 dnbinom(10L, 1L, mu = 1, log = TRUE))
    expect_equal(density_negative_binomial_mu(10L, 0, 1, FALSE, is_float),
                 suppressWarnings(dnbinom(10L, 0L, mu = 1, log = FALSE)))
    expect_equal(density_negative_binomial_mu(0L, 10, 1, TRUE, is_float),
                 suppressWarnings(dnbinom(0L, 10L, mu = 1, log = TRUE)),
                 tolerance = tolerance)
    ## We disagree with R here; we *could* return NaN but -Inf seems
    ## more sensible, and is what R returns if mu = eps
    expect_equal(density_negative_binomial_mu(10L, 0, 0, TRUE, is_float), -Inf)
    
    expect_equal(
      density_negative_binomial_mu(x = 0L, size = 2, mu = 0, log = TRUE,
                                   is_float = is_float),
      dnbinom(x = 0, size = 2, mu = 0, log = TRUE))
    expect_equal(
      density_negative_binomial_mu(x = 0L, size = 2, mu = 0, log = FALSE,
                                   is_float = is_float),
      dnbinom(x = 0, size = 2, mu = 0, log = FALSE))
    expect_equal(
      density_negative_binomial_mu(x = 1L, size = 2, mu = 0, log = TRUE,
                                   is_float = is_float),
      dnbinom(x = 1, size = 2, mu = 0, log = TRUE))
    expect_equal(
      density_negative_binomial_mu(x = 1L, size = 2, mu = 0, log = FALSE,
                                   is_float = is_float),
      dnbinom(x = 1, size = 2, mu = 0, log = FALSE))
    
    ## Special case where mu is zero
    expect_equal(
      dnbinom(34, 2, mu = 0, log = TRUE),
      density_negative_binomial_mu(34L, 2, 0, TRUE, is_float))
    expect_equal(
      dnbinom(34, 2, mu = 0, log = FALSE),
      density_negative_binomial_mu(34L, 2, 0, FALSE, is_float))
    
    ## Special case of mu << size
    expect_equal(density_negative_binomial_mu(0L, 50, 1e-8, TRUE, is_float),
                 dnbinom(0L, size = 50, mu = 1e-8, log = TRUE))
    expect_equal(density_negative_binomial_mu(0L, 50, 1e-20, TRUE, is_float),
                 dnbinom(0L, size = 50, mu = 1e-20, log = TRUE))
    
    ## Outside domain
    expect_identical(
      density_negative_binomial_mu(-1L, 5, 8, FALSE, is_float), 0)
    expect_identical(
      density_negative_binomial_mu(-1L, 5, 8, TRUE, is_float), -Inf)
  }
})


test_that("density::negative_binomial agrees in prob mode", {
  x <- as.integer(runif(50, max = 100))
  size <- runif(length(x), max = 50)
  prob <- runif(length(x))
  
  expect_equal(density_negative_binomial_prob(x, size, prob, TRUE),
               dnbinom(x, size, prob, log = TRUE))
  expect_equal(density_negative_binomial_prob(x, size, prob, FALSE),
               dnbinom(x, size, prob, log = FALSE))
  
  ## Corner cases
  expect_equal(density_negative_binomial_prob(0L, 5, 1, TRUE), 
               dnbinom(0, 5, 1, log = TRUE))
  expect_equal(density_negative_binomial_prob(0L, 5, 1, FALSE), 
               dnbinom(0, 5, 1, log = FALSE))
  expect_equal(density_negative_binomial_prob(1L, 5, 1, TRUE), 
               dnbinom(1, 5, 1, log = TRUE))
  expect_equal(density_negative_binomial_prob(1L, 5, 1, FALSE), 
               dnbinom(1, 5, 1, log = FALSE))
  expect_equal(density_negative_binomial_prob(0L, 0, 0.5, TRUE), 
               dnbinom(0, 0, 0.5, log = TRUE))
  expect_equal(density_negative_binomial_prob(0L, 0, 0.5, FALSE), 
               dnbinom(0, 0, 0.5, log = FALSE))
  expect_equal(density_negative_binomial_prob(1L, 0, 0.5, TRUE), 
               dnbinom(1, 0, 0.5, log = TRUE))
  expect_equal(density_negative_binomial_prob(1L, 0, 0.5, FALSE), 
               dnbinom(1, 0, 0.5, log = FALSE))
  
  ## Outside domain
  expect_identical(
    density_negative_binomial_prob(-1L, 5, 0.5, FALSE), 0)
  expect_identical(
    density_negative_binomial_prob(-1L, 5, 0.5, TRUE), -Inf)
})


test_that("density::beta_binomial agrees", {
  ## There's no beta-binomial in R stats so we'll create this here
  dbetabinom <- function(x, size, a, b, log = FALSE) {
    out <- lchoose(size, x) + lbeta(x + a, size - x + b) - lbeta(a, b)
    if (!log) {
      out <- exp(out)
    }
    out
  }
  
  size <- as.integer(0:50)
  prob <- runif(length(size))
  rho <- runif(length(size))
  a <- prob * (1 / rho - 1)
  b <- (1 - prob) * (1 / rho - 1)
  x <- as.integer(runif(length(size), 0, size))
  
  expect_equal(density_beta_binomial_prob(x, size, prob, rho, TRUE),
               dbetabinom(x, size, a, b, TRUE))
  expect_equal(density_beta_binomial_prob(x, size, prob, rho, FALSE),
               dbetabinom(x, size, a, b, FALSE))
  
  expect_equal(density_beta_binomial_ab(x, size, a, b, TRUE),
               dbetabinom(x, size, a, b, TRUE))
  expect_equal(density_beta_binomial_ab(x, size, a, b, FALSE),
               dbetabinom(x, size, a, b, FALSE))
  
  ## Corner cases
  expect_equal(density_beta_binomial_prob(0L, 0L, 0, 0, TRUE), 0)
  expect_equal(density_beta_binomial_prob(0L, 0L, 0.5, 0, FALSE), 1)
  expect_equal(density_beta_binomial_prob(0L, 0L, 0, 0, TRUE), 0)
  expect_equal(density_beta_binomial_prob(0L, 0L, 0.5, 0, FALSE), 1)
  
  expect_equal(density_beta_binomial_ab(0L, 0L, 0, 0, TRUE), 0)
  expect_equal(density_beta_binomial_ab(0L, 0L, 5, 0, FALSE), 1)
  expect_equal(density_beta_binomial_ab(0L, 0L, 0, 5, FALSE), 1)
  expect_equal(density_beta_binomial_ab(0L, 0L, 0, 0, TRUE), 0)
  expect_equal(density_beta_binomial_ab(0L, 0L, 5, 0, FALSE), 1)
  expect_equal(density_beta_binomial_ab(0L, 0L, 0, 5, FALSE), 1)
  
  ## Outside domain
  expect_identical(density_beta_binomial_prob(10L, 0L, 0.5, 0.1, FALSE), 0)
  expect_identical(density_beta_binomial_prob(10L, 2L, 0.5, 0.4, FALSE), 0)
  expect_identical(density_beta_binomial_prob(10L, 0L, 0.5, 0.1, TRUE), -Inf)
  expect_identical(density_beta_binomial_prob(10L, 2L, 0.5, 0.4, TRUE), -Inf)
  
  expect_identical(density_beta_binomial_ab(10L, 0L, 5, 1, FALSE), 0)
  expect_identical(density_beta_binomial_ab(10L, 2L, 5, 4, FALSE), 0)
  expect_identical(density_beta_binomial_ab(10L, 0L, 5, 1, TRUE), -Inf)
  expect_identical(density_beta_binomial_ab(10L, 2L, 5, 4, TRUE), -Inf)
})


test_that("density::uniform agrees", {
  min <- runif(50, -50, 50)
  max <- min + runif(50, 0, 100)
  x <- runif(length(min), min, max)
  expect_equal(dunif(x, min, max, TRUE),
               density_uniform(x, min, max, TRUE))
  expect_equal(dunif(x, min, max, FALSE),
               density_uniform(x, min, max, FALSE))
  
  ## Outside domain
  expect_identical(density_uniform(-1, 1, 2, FALSE), 0)
  expect_identical(density_uniform(3, 1, 2, FALSE), 0)
  expect_identical(density_uniform(-1, 1, 2, TRUE), -Inf)
  expect_identical(density_uniform(3, 1, 2, TRUE), -Inf)
})


test_that("density::beta agrees", {
  ## Similar to beta_binomial_ab, we will sample prob and rho on [0, 1]
  ## and convert to a and b
  prob <- runif(50)
  rho <- runif(length(prob))
  a <- prob * (1 / rho - 1)
  b <- (1 - prob) * (1 / rho - 1)
  x <- runif(length(prob))
  
  expect_equal(dbeta(x, a, b, log = TRUE),
               density_beta(x, a, b, TRUE))
  expect_equal(dbeta(x, a, b, log = FALSE),
               density_beta(x, a, b, FALSE))
  
  ## Outside domain
  expect_identical(density_beta(-1, 1, 2, FALSE), 0)
  expect_identical(density_beta(2, 1, 2, FALSE), 0)
    expect_identical(density_beta(-1, 1, 2, TRUE), -Inf)
  expect_identical(density_beta(2, 1, 2, TRUE), -Inf)
})


test_that("density::log_normal agrees", {
  mulog <- runif(50, -100, 100)
  sdlog <- runif(length(mulog), max = 100)
  x <- rlnorm(length(mulog), mulog, sdlog = runif(length(mulog), max = 100))
  expect_equal(dlnorm(x, mulog, sdlog, TRUE),
               density_log_normal(x, mulog, sdlog, TRUE))
  expect_equal(dlnorm(x, mulog, sdlog, FALSE),
               density_log_normal(x, mulog, sdlog, FALSE))
  
  ## Corner cases
  expect_equal(density_log_normal(1, 0, 0, TRUE), dlnorm(1, 0, 0, TRUE))
  expect_equal(density_log_normal(1, 0, 0, FALSE), dlnorm(1, 0, 0, FALSE))
  expect_equal(density_log_normal(2, 0, 0, TRUE), dlnorm(2, 0, 0, TRUE))
  expect_equal(density_log_normal(2, 0, 0, FALSE), dlnorm(2, 0, 0, FALSE))
  
  ## Outside domain
  expect_identical(density_log_normal(-1, 0, 1, FALSE), 0)
  expect_identical(density_log_normal(-1, 0, 1, TRUE), -Inf)
})


test_that("density::cauchy agrees", {
  location <- runif(50, -100, 100)
  x <- rcauchy(length(location), location, 
               scale = runif(length(location), max = 100))
  scale <- runif(length(x), max = 100)
  expect_equal(dcauchy(x, location, scale, TRUE),
               density_cauchy(x, location, scale, TRUE))
  expect_equal(dcauchy(x, location, scale, FALSE),
               density_cauchy(x, location, scale, FALSE))
  
  ## Corner cases
  ## dcauchy produces NaN here - do we want that?
  # expect_equal(density_cauchy(1, 1, 0, TRUE), dcauchy(1, 1, 0, TRUE))
  # expect_equal(density_cauchy(1, 1, 0, FALSE), dcauchy(1, 1, 0, FALSE))
  # expect_equal(density_cauchy(0, 1, 0, TRUE), dcauchy(0, 1, 0, TRUE))
  # expect_equal(density_cauchy(0, 1, 0, FALSE), dcauchy(0, 1, 0, FALSE))
})
