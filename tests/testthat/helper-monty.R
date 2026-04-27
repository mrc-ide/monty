ex_simple_gamma1 <- function(shape = 1, rate = 1) {
  e <- new.env(parent = .GlobalEnv)
  e$shape <- shape
  e$rate <- rate
  with(
    e,
    monty_model(
      list(
        parameters = "gamma",
        direct_sample = function(rng) {
          monty_random_gamma_scale(shape = shape, scale = 1 / rate, rng)
        },
        density = function(x) {
          drop(dgamma(x, shape = shape, rate = rate, log = TRUE))
        },
        gradient = function(x) drop((shape - 1) / x - rate),
        domain = rbind(c(0, Inf))),
      monty_model_properties(allow_multiple_parameters = TRUE)))
}


ex_simple_nested <- function(n_groups) {
  e <- new.env(parent = topenv())
  e$n_groups <- n_groups
  e$i <- as.numeric(seq_len(n_groups))
  with(
    e,
    monty_model(list(
      parameters = paste0("mu_", i),
      direct_sample = function(rng) {
        ## TODO: this is not great, we should allow coersion here in
        ## the C++ code nicely.
        vnapply(as.numeric(seq_len(n_groups)), function(i) {
          monty_random_normal(i, i, rng)
        })
      },
      density = function(x, by_group = FALSE) {
        z <- dnorm(x, 2^i, 1, log = TRUE)
        value <- sum(z)
        if (by_group) structure(value, "by_group" = z) else value
      },
      parameter_groups = i)))
}


## This is example is really stupid and does not really help test much
## except that the code can run at all. Ed/Marc; can you think of a
## reasonable example here please?
ex_simple_nested_with_base <- function(n_groups) {
  e <- new.env(parent = topenv())
  e$sigma <- 5
  e$mu <- rnorm(n_groups, 0, e$sigma)
  e$n_groups <- n_groups
  with(
    e,
    monty_model(list(
      parameters = c("sigma", paste0("mu_", seq_len(n_groups))),
      direct_sample = function(rng) {
        sigma <- monty_random_uniform(0, 10, rng)
        c(sigma, monty_random_n_normal(n_groups, 0, sigma, rng))
      },
      density = function(x, by_group = FALSE) {
        density1 <- function(y) {
          sigma <- y[[1]]
          if (sigma <= 0) {
            rep(-Inf, length(y) - 1)
          } else {
            dnorm(y[-1], 0, y[[1]], log = TRUE)
          }
        }
        if (is.matrix(x)) {
          z <- vapply(seq_len(ncol(x)), function(i) density1(x[, i]),
                      numeric(nrow(x) - 1))
          value <- colSums(z) + dunif(x[1, ], 0, 10, log = TRUE)
          if (by_group) structure(value, "by_group" = z) else value
        } else {
          z <- density1(x)
          value <- sum(z) + dunif(x[[1]], 0, 10, log = TRUE)
          if (by_group) structure(value, "by_group" = z) else value
        }
      },
      parameter_groups = c(0, seq_len(n_groups)),
      mu = mu),
      monty_model_properties(allow_multiple_parameters = TRUE)))
}


random_array <- function(dim, named = FALSE) {
  if (named) {
    dn <- lapply(seq_along(dim), function(i) {
      paste0(LETTERS[[i]], letters[seq_len(dim[i])])
    })
    names(dn) <- paste0("d", LETTERS[seq_along(dim)])
  } else {
    dn <- NULL
  }
  array(runif(prod(dim)), dim, dimnames = dn)
}


ex_sir_filter_likelihood <- function(n_particles = 100,
                                     deterministic = FALSE,
                                     save_trajectories = FALSE) {
  data <- data.frame(time      = c(4,  8, 12, 16, 20, 24, 28, 32, 36),
                     incidence = c(1,  0,  3,  5,  2,  4,  3,  7,  2))
  sir_filter_monty(data, n_particles, deterministic, save_trajectories)
}


ex_sir_filter_posterior <- function(...) {
  prior <- monty_dsl({
    beta ~ Gamma(shape = 1, rate = 1 / 0.5)
    gamma ~ Gamma(shape = 1, rate = 1 / 0.5)
  })
  ex_sir_filter_likelihood(...) + prior
}


## A silly stochastic model:
ex_stochastic <- function(n = 10, sd_sample = 1, sd_measure = 1) {
  env <- new.env()
  env$rng <- monty_rng_create()

  get_rng_state <- function() {
    monty_rng_state(env$rng)
  }
  set_rng_state <- function(rng_state) {
    monty_rng_set_state(rng_state, env$rng)
  }
  density <- function(x) {
    r <- monty_random_n_normal(n, x, sd_sample, env$rng)
    sum(dnorm(r, sd_measure, log = TRUE))
  }

  restore <- function() {
    env$rng <- monty_rng_create()
  }

  monty_model(
    list(env = env,
         density = density,
         restore = restore,
         parameters = "x",
         set_rng_state = set_rng_state,
         get_rng_state = get_rng_state),
    monty_model_properties(is_stochastic = TRUE))
}


## Here's Marc's original model, adapted to allow multiple parameters
## at once, which we will need here.
ex_mixture <- function(mu = 5) {
  monty_model(
    list(
      parameters = "x",
      density = function(x) {
        drop(log(0.5 * dnorm(x + mu) + 0.5 * dnorm(x - mu)))
      },
      gradient = function(x) {
        drop(-((x + mu) * dnorm(x + mu) + (x - mu) * dnorm(x - mu)) /
               (dnorm(x + mu) + dnorm(x - mu)))
      }),
    monty_model_properties(allow_multiple_parameters = TRUE))
}


scrub_manual_info <- function(x) {
  x <- sub("Manual monty sampling at '.+",
           "Manual monty sampling at '<PATH>'",
           x)
  x <- sub("Created .*[0-9]{4}.*",
           "Created <DATE>",
           x)
  x
}


drop_runner <- function(x) {
  x$restart$runner <- NULL
  x
}


ex_augmented <- function(p_group_1 = 0.25) {
  x <- local({
    n <- 100

    m1 <- 0
    m2 <- 3
    p <- 0.25

    x1 <- rnorm(n, m1)
    x2 <- rnorm(n, m2)
    z <- runif(n) < 0.25

    z * x1 + (1 - z) * x2
  })

  augmented_data_update <- function(pars, rng) {
    n <- length(x)

    z_new <- as.integer(monty_random_n_uniform(n, 0, 1, rng) < p_group_1)
    p <- pars[[1]]
    m1 <- pars[[2]]
    m2 <- pars[[3]]
    density_new <- likelihood_fn(p, m1, m2, x, z_new)

    z <- attr(pars, "data")
    if (is.null(z)) {
      z <- z_new
      density <- density_new
    } else {
      ## This recalculates the *old* density, per group.  For an
      ## expensive model that might not be ideal.
      density <- likelihood_fn(p, m1, m2, x, z)

      ## Our proposal ratio is not symmetric, we need to account for
      ## this here:
      proposal_ratio <- z * log(p_group_1) + (1 - z) * log(1 - p_group_1) -
        z_new * log(p_group_1) - (1 - z_new) * log(1 - p_group_1)
      p_accept <- pmin(1, exp(density_new - density + proposal_ratio))

      accept <- monty_random_n_uniform(n, 0, 1, rng) < p_accept
      if (any(accept)) {
        z[accept] <- z_new[accept]
        density[accept] <- density_new[accept]
      }
    }
    list(data = z, density = sum(density))
  }

  likelihood_fn <- function(p, m1, m2, x, z) {
    z * (log(p) + dnorm(x, m1, log = TRUE)) +
      (1 - z) * (log(1 - p) + dnorm(x, m2, log = TRUE))
  }

  prior <- monty_dsl({
    p ~ Beta(1, 1) # the same as Uniform(0, 1)
    m1 ~ Normal(0, 5)
    m2 ~ Normal(0, 5)
  })

  density <- function(pars) {
    p <- pars[[1]]
    m1 <- pars[[2]]
    m2 <- pars[[3]]
    z <- attr(pars, "data")
    sum(likelihood_fn(p, m1, m2, x, z))
  }

  likelihood <- monty_model(
    list(parameters = c("p", "m1", "m2"),
         density = density,
         augmented_data_update = augmented_data_update))

  list(likelihood = likelihood,
       prior = prior)
}
