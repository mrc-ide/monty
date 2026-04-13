test_that("Can run high level dsl function", {
  m <- monty_dsl("a ~ Normal(0, 1)")
  expect_s3_class(m, "monty_model")
  expect_equal(m$density(0), dnorm(0, 0, 1, log = TRUE))
  expect_equal(m$domain, rbind(a = c(-Inf, Inf)))
  expect_equal(m$gradient(0), 0)
})


test_that("can generate model with simple assignment", {
  x <- quote({
    mu <- 5
    a ~ Normal(mu, 1)
  })
  m <- monty_dsl(x)
  expect_s3_class(m, "monty_model")
  expect_equal(m$density(0), dnorm(0, 5, 1, log = TRUE))
  expect_equal(m$domain, rbind(a = c(-Inf, Inf)))
  expect_equal(m$gradient(0), 5)
  expect_equal(m$gradient(2), 3)
})


test_that("can sample from a simple model", {
  m <- monty_dsl("a ~ Normal(0, 1)")
  expect_s3_class(m, "monty_model")
  expect_true(m$properties$has_direct_sample)

  r <- monty_rng_create(seed = 42)
  cmp <- monty_random_normal(0, 1, r)

  r <- monty_rng_create(seed = 42)
  expect_equal(m$direct_sample(r), cmp)
})


test_that("can sample from a model with assignments", {
  m <- monty_dsl({
    mu <- 5
    a ~ Normal(mu, 1)
  })
  r <- monty_rng_create(seed = 42)
  cmp <- monty_random_normal(5, 1, r)

  r <- monty_rng_create(seed = 42)
  expect_equal(m$direct_sample(r), cmp)
})


test_that("Can compute domain for uniform distribution variables", {
  m <- monty_dsl({
    a <- 1
    x ~ Uniform(a, 2)
  })
  expect_equal(m$domain, rbind(x = c(1, 2)))
})


test_that("Can evaluate through chains of assignments to compute domain", {
  m <- monty_dsl({
    a <- 1
    b <- a * 2
    c <- b + a
    x ~ Uniform(a, c)
  })
  expect_equal(m$domain, rbind(x = c(1, 3)))
})


test_that("give up on bounds if they come from a stochastic process", {
  m <- monty_dsl({
    a <- 5
    b ~ Normal(a, 1)
    c ~ Uniform(a, b)
    d ~ Uniform(b, 10)
    e ~ Uniform(c, d)
  })
  expect_equal(m$domain,
               rbind(b = c(-Inf, Inf),
                     c = c(5, Inf),
                     d = c(-Inf, 10),
                     e = c(-Inf, Inf)))
})


test_that("can prevent creation of gradient function", {
  code <- "a ~ Normal(0, 1)"
  m1 <- monty_dsl(code, gradient = FALSE)
  expect_false(m1$properties$has_gradient)
  expect_null(m1$gradient)

  m2 <- monty_dsl(code, gradient = TRUE)
  expect_true(m2$properties$has_gradient)
  expect_true(is.function(m2$gradient))

  m3 <- monty_dsl(code, gradient = NULL)
  expect_true(m3$properties$has_gradient)
  expect_true(is.function(m3$gradient))
})


test_that("handle failure to create gradient function", {
  code <- "a ~ Normal(0, 1)\nb ~ Normal(trigamma(a), 1)"
  expect_no_warning(m1 <- monty_dsl(code, gradient = FALSE))
  expect_false(m1$properties$has_gradient)
  expect_null(m1$gradient)

  err <- expect_error(
    monty_dsl(code, gradient = TRUE),
    "Failed to differentiate this model")
  expect_s3_class(err$parent, "monty_differentiation_failure")

  w <- expect_warning(
    m3 <- monty_dsl(code, gradient = NULL),
    "Not creating a gradient function for this model")
  expect_s3_class(w$parent, "monty_parse_error")
  expect_s3_class(w$parent$parent, "monty_differentiation_failure")
  expect_false(m3$properties$has_gradient)
  expect_null(m3$gradient)
})


test_that("can compute gradients of complicated models", {
  m <- monty_dsl({
    a ~ Normal(0, 1)
    x <- a^2
    b ~ Exponential(2)
    c ~ Normal(x, b)
  })

  expect_equal(m$parameters, c("a", "b", "c"))
  expect_true(m$properties$has_gradient)

  p <- c(-.8, 0.03, 0.7)

  ## Silly density:
  expect_equal(
    m$density(p),
    dnorm(p[[1]], 0, 1, log = TRUE) +
    dexp(p[[2]], 2, log = TRUE) +
    dnorm(p[[3]], p[[1]]^2, p[[2]], log = TRUE))

  expect_equal(m$gradient(p), numDeriv::grad(m$density, p))
})


test_that("can use fixed data in dsl", {
  m <- monty_dsl({
    a ~ Normal(mu, sd)
  }, fixed = list(mu = 1, sd = 2))
  expect_equal(m$density(0), dnorm(0, 1, 2, log = TRUE))
})


test_that("can evaluate dsl model densities for multiple parameters", {
  m <- monty_dsl({
    a ~ Normal(0, 1)
    b ~ Exponential(2)
  })
  expect_true(m$properties$allow_multiple_parameters)
  x <- matrix(runif(10), 2, 5)
  expect_equal(m$density(x),
               dnorm(x[1, ], 0, 1, TRUE) + dexp(x[2, ], 2, TRUE))
  expect_equal(
    m$gradient(x),
    apply(x, 2, m$gradient))
  expect_equal(
    m$gradient(x[, 1, drop = FALSE]),
    cbind(m$gradient(x[, 1])))
})


test_that("gradient calculation correct single-parameter model", {
  m <- monty_dsl({
    a ~ Normal(0, 1)
  })
  expect_true(m$properties$allow_multiple_parameters)
  x <- matrix(runif(5), 1, 5)
  expect_equal(m$density(x),
               dnorm(x[1, ], 0, 1, TRUE))
  expect_equal(
    m$gradient(x),
    rbind(apply(x, 2, m$gradient)))
  expect_equal(
    m$gradient(x[, 1, drop = FALSE]),
    cbind(m$gradient(x[, 1])))
})


test_that("can apply a domain", {
  m <- monty_dsl({
    a ~ Exponential(1)
  },
  domain = rbind(a = c(1, 4)))
  expect_equal(m$density(2), -2)
  expect_equal(m$density(0), -Inf)
  expect_equal(m$density(6), -Inf)
})


test_that("can apply a domain", {
  m <- monty_dsl({
    a ~ Exponential(1)
    b ~ Normal(0, 1)
  },
  domain = rbind(a = c(1, 4), b = c(-2, 2)))
  expect_equal(m$density(c(2, 1)),
               dexp(2, log = TRUE) + dnorm(1, log = TRUE))
  expect_equal(m$density(c(0, 1)), -Inf)
  expect_equal(m$density(c(1, 6)), -Inf)
})


test_that("can use truncated normal in dsl", {
  expect_warning(
    prior <- monty_dsl({
      beta ~ TruncatedNormal(mean = 0.2, sd = 0.1, min = 0.05, max = 0.5)
    }),
    "Not creating a gradient function for this model")
  expect_s3_class(prior, "monty_model")
  expect_false(prior$properties$has_gradient)
})


test_that("can use arrays in dsl", {
  ## 1d arrays
  expect_warning(
    m <- monty_dsl({
      lambda[] <- 2 * i
      x[] ~ Exponential(lambda[i])
      dim(x, lambda) <- 3
    }),
    "Not creating a gradient function for this model")
  expect_s3_class(m, "monty_model")
  expect_false(m$properties$has_gradient)
  expect_equal(m$parameters, c("x[1]", "x[2]", "x[3]"))
  expect_equal(m$density(seq_len(3)), 
               sum(dexp(seq_len(3), 2 * seq_len(3), log = TRUE)))
  domain <- rbind(c(0, Inf), c(0, Inf), c(0, Inf))
  rownames(domain) <- m$parameters
  expect_equal(m$domain, domain)
  expect_equal(m$gradient, NULL)
  
  r <- monty_rng_create(seed = 42)
  cmp <- vnapply(c(2, 4, 6), function(x) monty_random_exponential_rate(x, r))
  
  r <- monty_rng_create(seed = 42)
  expect_equal(m$direct_sample(r), cmp)
  
  
  ## 1d arrays with multiline
  expect_warning(
    m2 <- monty_dsl({
      lambda[1:2] <- 2 * i
      lambda[3] <- 2 * i + 1
      x[1] ~ Exponential(lambda[i])
      x[2:3] ~ Exponential(lambda[i]^2)
      dim(x, lambda) <- n
    }, fixed = list(n = 3)),
    "Not creating a gradient function for this model")
  expect_s3_class(m2, "monty_model")
  expect_false(m2$properties$has_gradient)
  expect_equal(m2$parameters, c("x[1]", "x[2]", "x[3]"))
  expect_equal(m2$density(seq_len(3)), 
               sum(dexp(seq_len(3), c(2, 16, 49), log = TRUE)))
  domain <- rbind(c(0, Inf), c(0, Inf), c(0, Inf))
  rownames(domain) <- m2$parameters
  expect_equal(m2$domain, domain)
  expect_equal(m2$gradient, NULL)
  
  r <- monty_rng_create(seed = 42)
  cmp <- vnapply(c(2, 16, 49), function(x) monty_random_exponential_rate(x, r))
  
  r <- monty_rng_create(seed = 42)
  expect_equal(m2$direct_sample(r), cmp)
  
  
  ## sequentially-dependent 1d arrays
  expect_warning(
    m <- monty_dsl({
      lambda[1] <- 1
      lambda[2:3] <- lambda[i - 1] + 1
      x[1] ~ Normal(0, lambda[i])
      x[2:3] ~ Normal(x[i - 1], lambda[i])
      dim(x, lambda) <- 3
    }),
    "Not creating a gradient function for this model")
  expect_s3_class(m, "monty_model")
  expect_false(m$properties$has_gradient)
  expect_equal(m$parameters, c("x[1]", "x[2]", "x[3]"))
  expect_equal(m$density(seq_len(3)), 
               sum(dnorm(seq_len(3), c(0, 1, 2), seq_len(3), log = TRUE)))
  domain <- rbind(c(-Inf, Inf), c(-Inf, Inf), c(-Inf, Inf))
  rownames(domain) <- m$parameters
  expect_equal(m$domain, domain)
  expect_equal(m$gradient, NULL)
  
  r <- monty_rng_create(seed = 42)
  cmp <- numeric(3)
  cmp[1] <- monty_random_normal(0, 1, r)
  cmp[2] <- monty_random_normal(cmp[1], 2, r)
  cmp[3] <- monty_random_normal(cmp[2], 3, r)
  
  r <- monty_rng_create(seed = 42)
  expect_equal(m$direct_sample(r), cmp)
  
  
  ## 2d arrays
  expect_warning(
    m3 <- monty_dsl({
      lambda[, ] <- 2 * i + j
      x[, ] ~ Exponential(lambda[i, j])
      dim(x, lambda) <- c(2, 3)
    }),
    "Not creating a gradient function for this model")
  expect_s3_class(m3, "monty_model")
  expect_false(m3$properties$has_gradient)
  expect_equal(m3$parameters, c("x[1,1]", "x[2,1]", 
                                "x[1,2]", "x[2,2]",
                                "x[1,3]", "x[2,3]"))
  ## calculate lambda[i, j] = 2 * i + j
  lambda <- array(2 * seq_len(2), c(2, 3)) + t(array(seq_len(3), c(3, 2)))
  
  expect_equal(m3$density(seq_len(6)), 
               sum(dexp(seq_len(6), c(lambda), log = TRUE)))
  domain <- t(array(c(0, Inf), c(2, 6)))
  rownames(domain) <- m3$parameters
  expect_equal(m3$domain, domain)
  expect_equal(m3$gradient, NULL)
  
  ## direct sampling will have i as the outer loop and j as the inner loop
  ## so we need to sample in that order and then match the model's parameter
  ## order
  r <- monty_rng_create(seed = 42)
  cmp <- 
    rbind(vnapply(lambda[1, ], function(x) monty_random_exponential_rate(x, r)),
          vnapply(lambda[2, ], function(x) monty_random_exponential_rate(x, r)))
  
  r <- monty_rng_create(seed = 42)
  expect_equal(m3$direct_sample(r), c(cmp))
})


test_that("cannot use reserved words in fixed", {
  expect_error(
    monty_dsl({a <- 1}, 
              fixed = list(i = 1, j = 2, dim = 3, dim_a = 4)),
    "Element names 'i', 'j', 'dim', and 'dim_a' in 'fixed' not allowed",
    fixed = TRUE)
})
