##' Create a Hamiltonian Monte Carlo sampler, implemented using the
##' leapfrog algorithm.
##'
##' @title Create HMC
##'
##' @param epsilon The step size of the HMC steps
##'
##' @param n_integration_steps The number of HMC steps per step
##'
##' @param vcv A variance-covariance matrix for the momentum vector.
##'   The default uses an identity matrix.
##'
##' @param debug Logical, indicating if we should save all
##'   intermediate points and their gradients.  This will add a vector
##'   "history" to the details after the integration.  This *will*
##'   slow things down though as we accumulate the history
##'   inefficiently.
##'
##' @return A `monty_sampler` object, which can be used with
##'   [monty_sample]
##'
##' @export
monty_sampler_hmc <- function(epsilon = 0.015, n_integration_steps = 10,
                              vcv = NULL, debug = FALSE) {
  if (!is.null(vcv)) {
    check_vcv(vcv, call = environment())
  }
  assert_scalar_size(n_integration_steps, allow_zero = FALSE)
  assert_scalar_numeric(epsilon)
  assert_scalar_logical(debug)

  control <- list(epsilon = epsilon,
                  n_integration_steps = n_integration_steps,
                  vcv = vcv,
                  debug = debug)

  properties <- monty_sampler_properties(
    allow_multiple_parameters = TRUE,
    requires_gradient = TRUE,
    requires_deterministic = TRUE)

  monty_sampler("Hamiltonian Monte Carlo",
                "monty_sampler_hmc",
                control,
                sampler_hmc_initialise,
                sampler_hmc_step,
                sampler_hmc_dump,
                sampler_hmc_combine,
                sampler_hmc_restore,
                sampler_hmc_details,
                properties = properties)
}


sampler_hmc_initialise <- function(state_chain, control, model, rng) {
  pars <- state_chain$pars
  list(transform = hmc_transform(model, pars),
       sample_momentum = hmc_momentum(control, model, pars),
       history = hmc_history_recorder(control, pars))
}


sampler_hmc_step <- function(state_chain, state_sampler, control, model, rng) {
  gradient <- function(x) {
    state_sampler$transform$deriv(x) * model$gradient(x)
  }

  pars <- state_chain$pars
  theta <- state_sampler$transform$model2rn(pars)

  if (control$debug) {
    state_sampler$history$add(0, pars)
  }

  v <- state_sampler$sample_momentum(rng)

  ## Compute kinetic energy at the start; we'll compare with this later at
  ## the acceptance test.
  energy0 <- hmc_kinetic_energy(v)

  ## Make a half step for momentum at the beginning
  v <- v + control$epsilon * gradient(pars) / 2

  for (i in seq_len(control$n_integration_steps)) {
    ## Make a full step for the position
    theta <- theta + control$epsilon * v
    pars <- state_sampler$transform$rn2model(theta)
    ## Make a full step for the momentum, except at end of trajectory
    if (i != control$n_integration_steps) {
      v <- v + control$epsilon * gradient(pars)
    }
    if (control$debug) {
      state_sampler$history$add(i, pars)
    }
  }

  ## Make a half step for momentum at the end.
  v <- v + control$epsilon * gradient(pars) / 2

  ## Some treatments would negate momentum at end of trajectory to
  ## make the proposal symmetric by setting `v <- -v` but
  ## this is not actually needed in practice because we never
  ## reference v again.

  density_next <- model$density(pars)
  energy_next <- hmc_kinetic_energy(v)

  ## Accept or reject the state at end of trajectory, returning
  ## either the position at the end of the trajectory or the initial
  ## position
  u <- monty_random_real(rng)
  accept <- u < exp(density_next - state_chain$density + energy0 - energy_next)

  if (control$debug) {
    state_sampler$history$complete(accept)
  }

  update_state(state_chain, pars, density_next, accept, model)
}


sampler_hmc_dump <- function(state, control) {
  history <- state$history$dump()
  if (length(history) == 0) {
    return(NULL)
  }
  history
}


sampler_hmc_combine <- function(state, control) {
  if (all(vlapply(state, is.null))) {
    return(NULL)
  }

  join <- function(name, ...) {
    array_bind(arrays = lapply(state, "[[", name), ...)
  }

  ## With the parallel tempering algorithm we already have more than
  ## one chain worth and we need to combine differently, pushing the
  ## apparent chain (which really means rung) *before* the state.  We
  ## need to do this at combine, rather than dump, because the dump is
  ## also used in the simultaneous sampler.
  is_parallel_tempering <- ncol(state[[1]]$accept) > 1
  if (is_parallel_tempering) {
    pars <- aperm(join("pars", on = 5), c(1, 2, 4, 3, 5))
    accept <- aperm(join("accept", on = 3), c(2, 1, 3))
  } else {
    pars <- join("pars", on = 4)
    accept <- join("accept", on = 2)
  }

  list(pars = pars, accept = accept)
}


sampler_hmc_restore <- function(chain_id, state_chain, state_sampler, control,
                                model) {
  pars <- state_chain$pars

  if (is.null(state_sampler)) {
    history <- NULL
  } else {
    is_parallel_tempering <- length(dim(state_sampler$accept)) > 2
    if (is_parallel_tempering) {
      history <- list(
        pars = state_sampler$pars[, , , , chain_id, drop = FALSE],
        accept = state_sampler$accept[, , chain_id, drop = FALSE])
    } else {
      history <- list(
        pars = state_sampler$pars[, , , chain_id, drop = FALSE],
        accept = state_sampler$accept[, chain_id, drop = FALSE])
    }
  }
  list(transform = hmc_transform(model, pars),
       sample_momentum = hmc_momentum(control, model, pars),
       history = hmc_history_recorder(control, pars, history))
}


sampler_hmc_details <- function(state, control) {
  state
}


hmc_momentum <- function(control, model, pars) {
  if (is.null(control$vcv)) {
    n_pars <- length(model$parameters)
    sample_momentum <- function(rng) {
      monty_random_n_normal(n_pars, 0, 1, rng)
    }
  } else {
    vcv <- sampler_validate_vcv(control$vcv, pars)
    make_rmvnorm(vcv)
  }
}


hmc_transform <- function(model, pars) {
  multiple_parameters <- length(dim2(pars)) > 1
  hmc_transform_fn(model$domain, multiple_parameters)
}


hmc_transform_fn <- function(domain, multiple_parameters) {
  lower <- domain[, 1]
  upper <- domain[, 2]

  is_bounded <- is.finite(lower) & is.finite(upper)
  is_semi_infinite <- is.finite(lower) & upper == Inf
  is_infinite <- lower == -Inf & upper == Inf
  unknown <- !(is_bounded | is_semi_infinite | is_infinite)
  if (any(unknown)) {
    ## This is not very likely to be thrown
    cli::cli_abort("Unhandled domain type for parameter {which(unknown)}")
  }

  ## Save finite bounds for later:
  a <- lower[is_bounded]
  b <- upper[is_bounded]

  if (multiple_parameters) {
    rn2model <- function(theta) {
      theta[is_semi_infinite, ] <-
        lower[is_semi_infinite] + exp(theta[is_semi_infinite, ])
      theta[is_bounded, ] <- ilogit_bounded(theta[is_bounded, ], a, b)
      theta
    }

    model2rn <- function(x) {
      x[is_semi_infinite, ] <-
        log(x[is_semi_infinite, ] - lower[is_semi_infinite])
      x[is_bounded, ] <- logit_bounded(x[is_bounded, ], a, b)
      x
    }

    ## Derivative of rn2model, as a multiplier to f'(x); we pass in
    ## model parameters here, not R^n space.
    deriv <- function(x) {
      ret <- matrix(1, nrow(x), ncol(x))
      ## Here we want
      ## d/dtheta f(exp(theta)) -> exp(theta) f'(exp(theta))
      ##                        -> x f'(x)
      ret[is_semi_infinite, ] <- x[is_semi_infinite, ]
      ## See below, this one is a bit harder
      ret[is_bounded, ] <- dilogit_bounded(x[is_bounded, ], a, b)
      ret
    }
  } else {
    rn2model <- function(theta) {
      theta[is_semi_infinite] <-
        lower[is_semi_infinite] + exp(theta[is_semi_infinite])
      theta[is_bounded] <- ilogit_bounded(theta[is_bounded], a, b)
      theta
    }

    model2rn <- function(x) {
      x[is_semi_infinite] <- log(x[is_semi_infinite] - lower[is_semi_infinite])
      x[is_bounded] <- logit_bounded(x[is_bounded], a, b)
      x
    }

    ## Derivative of rn2model, as a multiplier to f'(x); we pass in
    ## model parameters here, not R^n space.
    deriv <- function(x) {
      ret <- numeric(length(x))
      ## d/dtheta theta -> 1
      ret[is_infinite] <- 1
      ## Here we want
      ## d/dtheta f(exp(theta)) -> exp(theta) f'(exp(theta))
      ##                        -> x f'(x)
      ret[is_semi_infinite] <- x[is_semi_infinite]
      ## See below, this one is a bit harder
      ret[is_bounded] <- dilogit_bounded(x[is_bounded], a, b)
      ret
    }
  }

  list(
    ## Transform from R^n into the model space
    rn2model = rn2model,
    ## Transform from model space to R^n
    model2rn = model2rn,
    ## Derivative of rn2model, as a multiplier to f'(x); we pass in
    ## model parameters here, not R^n space.
    deriv = deriv)
}


## Convert (0, 1) or (a, b) to (-Inf, Inf)
##
## Typically this is model -> R^n
logit_bounded <- function(x, a, b) {
  p <- (x - a) / (b - a)
  log(p / (1 - p))
}


## Convert (-Inf, Inf) to (0, 1) or (a, b)
##
## Typically this is R^n -> model
ilogit_bounded <- function(theta, a, b) {
  p <- exp(theta) / (1 + exp(theta))
  p * (b - a) + a
}


## The chain rule is d/dtheta f(g(theta)) -> g(theta) f'(g(theta))
##
## our g is the translation theta -> x which is
##
## >  a + (b - a) exp(theta) / (1 + exp(theta))
##
## The derivative of this with respect to theta:
##
## > (b - a) exp(theta) / (exp(theta) + 1)^2
##
## Substituting the theta -> x transformation log((x - a) / (b - x)) into this
##
## > (b - a) exp(log((x - a) / (b - x))) / (exp(log((x - a) / (b - x))) + 1)^2
## > (a - x) * (b - x) / (a - b)
##
## In the case a = 0, b = 1 we get x * (1 - x) which we expect
##
## https://en.wikipedia.org/wiki/Logistic_function#Derivative
dilogit_bounded <- function(x, a, b) {
  (a - x) * (b - x) / (a - b)
}


hmc_kinetic_energy <- function(v) {
  if (length(dim(v)) < 2) {
    sum(v^2) / 2
  } else {
    colSums(v^2) / 2
  }
}


hmc_history_recorder <- function(control, pars, history = NULL) {
  if (!control$debug) {
    return(list(add = function(i, pars) NULL,
                complete = function(accept) NULL,
                details = function() NULL,
                dump = function() NULL))
  }

  if (length(dim2(pars)) > 1) {
    n_pars <- nrow(pars)
    n_sets <- ncol(pars)
  } else {
    n_pars <- length(pars)
    n_sets <- 1
  }
  n_integration <- control$n_integration_steps + 1L

  env <- new.env(parent = emptyenv())
  if (is.null(history)) {
    env$pars <- numeric()
    env$accept <- logical()
  } else if (n_sets == 1) {
    env$pars <- as.vector(history$pars)
    env$accept <- as.vector(history$accept)
  } else {
    is_parallel_tempering <- length(dim(history$accept)) > 2
    if (is_parallel_tempering) {
      env$pars <- as.vector(aperm(history$pars, c(1, 2, 4, 3, 5)))
      env$accept <- as.vector(history$accept)
    } else {
      env$pars <- as.vector(aperm(history$pars, c(1, 4, 2, 3)))
      env$accept <- as.vector(t(history$accept))
    }
    
  }

  add <- function(i, pars) {
    if (i == 0) {
      env$curr <- array(NA_real_, c(n_pars, n_sets, n_integration))
    }
    env$curr[, , i + 1L] <- pars
  }

  complete <- function(accept) {
    env$pars <- c(env$pars, env$curr)
    env$accept <- c(env$accept, accept)
  }

  dump <- function() {
    n_mcmc <- length(env$accept) / n_sets
    pars <- array(env$pars, c(n_pars, n_sets, n_integration, n_mcmc))
    pars <- aperm(pars, c(1, 3, 4, 2))
    list(pars = pars,
         accept = matrix(env$accept, ncol = n_sets, byrow = TRUE))
  }

  list(add = add,
       complete = complete,
       dump = dump)
}
