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
  internal <- new.env()

  if (!is.null(vcv)) {
    check_vcv(vcv, call = environment())
  }

  initialise <- function(pars, model, rng) {
    require_deterministic(model, "Can't use HMC with stochastic models")
    require_gradient(model, "Can't use HMC without a gradient")

    internal$multiple_parameters <- length(dim2(pars)) > 1
    internal$transform <- hmc_transform(model$domain,
                                        internal$multiple_parameters)
    n_sets <- if (internal$multiple_parameters) ncol(pars) else 1L
    n_pars <- if (internal$multiple_parameters) nrow(pars) else length(pars)
    if (is.null(vcv)) {
      internal$sample_momentum <- function(rng) {
        monty_random_n_normal(n_pars, 0, 1, rng)
      }
    } else {
      vcv <- sampler_validate_vcv(vcv, pars)
      internal$sample_momentum <- make_rmvnorm(vcv)
    }
    if (debug) {
      internal$history <- list()
    }
    internal$n_sets <- n_sets
    initialise_state(pars, model, rng)
  }

  step <- function(state, model, rng) {
    ## Just a helper for now
    compute_gradient <- function(x) {
      internal$transform$deriv(x) * model$gradient(x)
    }

    theta <- internal$transform$model2rn(state$pars)
    pars_next <- state$pars
    theta_next <- theta

    if (debug) {
      n_sets <- internal$n_sets
      n_pars <- length(model$parameters)
      history <- array(NA_real_, c(n_pars, n_sets, n_integration_steps + 1))
      history[, , 1] <- pars_next
    }

    v <- internal$sample_momentum(rng)

    ## Make a half step for momentum at the beginning
    v_next <- v + epsilon * compute_gradient(pars_next) / 2

    for (i in seq_len(n_integration_steps)) {
      ## Make a full step for the position
      theta_next <- theta_next + epsilon * v_next
      pars_next <- internal$transform$rn2model(theta_next)
      ## Make a full step for the momentum, except at end of trajectory
      if (i != n_integration_steps) {
        v_next <- v_next + epsilon * compute_gradient(pars_next)
      }
      if (debug) {
        history[, , i + 1] <- pars_next
      }
    }

    ## Make a half step for momentum at the end.
    v_next <- v_next + epsilon * compute_gradient(pars_next) / 2

    ## Some treatments would negate momentum at end of trajectory to
    ## make the proposal symmetric by setting v_next <- -v_next but
    ## this is not actually needed in practice.

    ## Potential energy at the end of the trajectory
    density_next <- model$density(pars_next)

    ## Kinetic energies at the start (energy) and end (energy_next) of
    ## the trajectories
    if (internal$multiple_parameters) {
      energy <- colSums(v^2) / 2
      energy_next <- colSums(v_next^2) / 2
    } else {
      energy <- sum(v^2) / 2
      energy_next <- sum(v_next^2) / 2
    }

    ## Accept or reject the state at end of trajectory, returning
    ## either the position at the end of the trajectory or the initial
    ## position
    u <- monty_random_real(rng)
    accept <- u < exp(density_next - state$density + energy - energy_next)

    if (debug) {
      internal$history <-
        c(internal$history, list(list(pars = history, accept = accept)))
    }

    state <- update_state(state, pars_next, density_next, accept,
                          model, rng)

    state
  }

  finalise <- function(state, model, rng) {
    if (debug) {
      pars <- array_bind(arrays = lapply(internal$history, "[[", "pars"),
                         after = 3)
      accept <- lapply(internal$history, function(x) x$accept)
      if (internal$multiple_parameters) {
        pars <- aperm(pars, c(1, 3, 4, 2))
        accept <- matrix(unlist(accept), ncol = internal$n_sets, byrow = TRUE)
      } else {
        dim(pars) <- dim(pars)[-2] # redundant sets dimension
        accept <- vlapply(accept, identity)
      }
      rownames(pars) <- model$parameters
      list(pars = pars, accept = accept)
    } else {
      NULL
    }
  }

  get_internal_state <- function() {
    if (!debug) {
      return(NULL)
    }
    history <- internal$history
    if (internal$multiple_parameters) {
      ## Split this by chain:
      lapply(seq_len(internal$n_sets), function(i) {
        lapply(history, function(x) {
          list(pars = x$pars[, i, , drop = FALSE], accept = x$accept[[i]])
        })
      })
    } else {
      history
    }
  }

  set_internal_state <- function(state) {
    if (internal$multiple_parameters && !is.null(state)) {
      ## Fairly ugly transpose here; there will be other ways of doing
      ## this to explore later.
      internal$history <- lapply(seq_len(length(state[[1]])), function(j) {
        list(pars = array_bind(arrays = lapply(state, function(x) x[[j]]$pars),
                               on = 2),
             accept = vlapply(state, function(x) x[[j]]$accept))
      })
    } else {
      internal$history <- state
    }
  }

  monty_sampler("Hamiltonian Monte Carlo",
                "monty_sampler_hmc",
                initialise,
                step,
                finalise,
                get_internal_state,
                set_internal_state)
}


hmc_transform <- function(domain, multiple_parameters) {
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
