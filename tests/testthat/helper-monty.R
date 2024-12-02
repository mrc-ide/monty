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
  data <- data.frame(time      = c( 4,  8, 12, 16, 20, 24, 28, 32, 36),
                     incidence = c( 1,  0,  3,  5,  2,  4,  3,  7,  2))
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
    monty_rng_set_state(rng_state, rng)
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


scrub_manual_info <- function(x) {
  x <- sub("Manual monty sampling at '.+",
           "Manual monty sampling at '<PATH>'",
           x)
  x <- sub("Created .*[0-9]{4}.*",
           "Created <DATE>",
           x)
  x
}
