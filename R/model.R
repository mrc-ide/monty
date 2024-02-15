## Nothing we have in R provides the proper concepts of an interface
## unfortunately, but these are all pretty coupled
mcstate_model <- function(parameters, sample, density, gradient, domain) {
  ret <- list(parameters = parameters,
              sample = sample,
              density = density,
              gradient = gradient,
              domain = domain)
  class(ret) <- "mcstate_model"
  ret
}


## Probably we'll do something slightly more compositional here, by
## pulling together a "sampler" object that can initialise itself and
## draw samples.
##
## Let's write out three basic samplers;
##
## * the most basic Metropolis-Hastings sampler (as in mcstate1)
## * the adadaptive step size algorithm that Marc wrote
## * an HMC algorithm
##
## then we'll work out if we can split this into some reusable chunks

## Simple MH
mcstate_sample_mh <- function(model, proposal, n_steps, initial) {
  ## TODO: we might use an isolated rng stream here
  if (is.null(initial)) {
    ## Really this would just be from the prior; we can't directly
    ## sample from the posterior!
    pars <- model$sample()
  } else {
    ## Check here that the initial parameters make sense probably
    pars <- initial
  }

  density <- model$density(pars)

  ## This will certainly be done by some common code
  history_pars <- matrix(NA_real_, n_steps + 1, length(pars))
  history_pars[1, ] <- pars
  history_density <- rep(NA_real_, n_steps + 1)
  history_density[[1]] <- density

  for (i in seq_len(n_steps)) {
    u <- runif(1)
    density_accept <- density + log(u)
    pars_next <- proposal(pars)
    ## We might want to do a few things here to pass in density_accept,
    ## and to indicate that we want to use the model (if provided) and
    ## not the prior here.  We might also want to report all of the
    ## three probabilities here, as at the moment we don't allow that
    ## decomposition.
    density_next <- model$density(pars_next)
    if (density_next > density_accept) {
      pars <- pars_next
      density <- density_next
    }

    history_pars[i + 1, ] <- pars
    history_density[[i + 1]] <- density
  }

  ## Pop the parameter names on last
  colnames(history_pars) <- model$parameters

  list(pars = history_pars,
       density = history_density)
}


## Adaptive step size
mcstate_sample_mh_adaptive <- function(model, vcv, n_steps, initial) {
  ## These will be tuneable later
  initial_scaling <- 0.2
  scaling_increment <- 0.01
  acceptance_target <- 0.234
  initial_weight <- 1000
  adaptive_contribution <- 0.95

  if (is.null(initial)) {
    pars <- model$sample()
    initial <- pars
  }

  vcv_initial <- vcv
  pars <- initial %||% model$sample()
  scaling <- initial_scaling
  weight <- initial_weight

  mean <- initial
  autocorrelation <- vcv + weight / (weight - 1) * qp(mean)

  density <- model$density(pars)

  ## This will certainly be done by some common code
  history_pars <- matrix(NA_real_, n_steps + 1, length(pars))
  history_pars[1, ] <- pars
  history_density <- rep(NA_real_, n_steps + 1)
  history_density[[1]] <- density

  for (i in seq_len(n_steps)) {
    proposal_was_adaptive <- runif(1) < adaptive_contribution
    if (proposal_was_adaptive) {
      vcv <- scaling * (autocorrelation - weight / (weight - 1) * qp(mean))
      pars_next <- rmvnorm(pars, vcv)
    } else {
      pars_next <- rmvnorm(pars, vcv_initial)
    }

    u <- runif(1)
    density_accept <- density + log(u)
    density_next <- model$density(pars_next)

    accept <- density_next > density_accept
    if (accept) {
      pars <- pars_next
      density <- density_next
    }

    history_pars[i + 1, ] <- pars
    history_density[[i + 1]] <- density

    if (proposal_was_adaptive) {
      if (accept) {
        scaling <- scaling + (1 - acceptance_target) * scaling_increment
      } else {
        scaling <- max(scaling - acceptance_target * scaling_increment,
                       scaling_increment)
      }

      ## Update of the autocorrelation matrix and mean of past samples
      weight <- weight + 1
      autocorrelation <- (1 - 1 / (weight - 1)) * autocorrelation +
        1 / (weight - 1) * qp(pars)
      mean <- (1 - 1 / weight) * mean + 1 / weight * pars
    }
  }

  ## Pop the parameter names on last
  colnames(history_pars) <- model$parameters

  list(pars = history_pars,
       density = history_density)
}


mcstate_sample_hmc <- function(model, n_steps, initial) {
  epsilon <- 0.015
  n_integration_steps <- 10 # "L" in the literature

  if (is.null(initial)) {
    ## Really this would just be from the prior; we can't directly
    ## sample from the posterior!
    pars <- model$sample()
  } else {
    ## Check here that the initial parameters make sense probably
    pars <- initial
  }

  transform <- hmc_transform(model$domain)

  if (is.null(initial)) {
    pars <- model$sample()
  }

  theta <- transform$model2rn(pars)

  density <- model$density(pars)
  n_parameters <- length(pars)

  ## This computes the gradient with respect to the _transformed_
  ## parameters from the gradient; basically more chain rule again.  I
  ## think that this might need to change later once we have different
  ## functions though.
  compute_gradient_from_rn <- function(x) {
    transform$deriv(x) * model$gradient(x)
  }

  density <- model$density(pars)

  history_pars <- matrix(NA_real_, n_steps + 1, length(pars))
  history_pars[1, ] <- pars
  history_density <- rep(NA_real_, n_steps + 1)
  history_density[[1]] <- density

  for (i in seq_len(n_steps)) {
    v <- rnorm(n_parameters, 0, 1) # may want a vcv here in future
    pars_next <- pars
    theta_next <- theta

    ## Make a half step for momentum at the beginning
    v_next <- v + epsilon * compute_gradient_from_rn(pars_next) / 2

    for (j in seq_len(n_integration_steps)) {
      ## Make a full step for the position
      theta_next <- theta_next + epsilon * v_next
      pars_next <- transform$rn2model(theta_next)
      ## Make a full step for the momentum, except at end of trajectory
      if (j != n_integration_steps) {
        v_next <- v_next + epsilon * compute_gradient_from_rn(pars_next)
      }
    }

    ## Make a half step for momentum at the end.
    v_next <- v_next + epsilon * compute_gradient_from_rn(pars_next) / 2

    ## Negate momentum at end of trajectory to make the proposal symmetric
    v_next <- -v_next

    ## Potential energy at the end of the trajectory
    density_next <- model$density(pars_next)

    ## Kinetic energies at the start (energy) and end (energy_next) of
    ## the trajectories
    energy <- sum(v^2) / 2
    energy_next <- sum(v_next^2) / 2

    # Accept or reject the state at end of trajectory, returning
    # either the position at the end of the trajectory or the initial
    # position
    u <- runif(1)
    accept <- u < exp(density_next - density + energy - energy_next)
    if (accept) {
      density <- density_next
      pars <- pars_next
      theta <- theta_next
    }

    history_pars[i + 1, ] <- pars
    history_density[[i + 1]] <- density
  }

  ## Pop the parameter names on last
  colnames(history_pars) <- model$parameters

  list(pars = history_pars,
       density = history_density)
}


qp <- function(x) {
  outer(x, x)
}

## This is one way of doing samples from a multivariate normal, but
## it's not great as sweep is slow.
##
## rmvnorm <- function(x, vcv) {
##   drop(mvtnorm::rmvnorm(1, x, vcv))
## }


rmvnorm <- function(x, vcv) {
  r <- chol(vcv, pivot = TRUE)
  r <- r[, order(attr(r, "pivot", exact = TRUE))]
  x + drop(rnorm(ncol(vcv)) %*% r)
}


hmc_transform <- function(domain) {
  is_bounded <- is.finite(domain[, 1]) & is.finite(domain[, 2])
  is_semi_infinite <- is.finite(domain[, 1]) & domain[, 2] == Inf
  is_infinite <- domain[, 1] == -Inf & domain[, 2] == Inf
  unknown <- !(is_bounded | is_semi_infinite | is_infinite)
  if (any(unknown)) {
    stop("Can't handle this")
  }
  if (any(is_bounded)) {
    stop("write a logit transform")
  }
  list(
    rn2model = function(x) {
      x[is_semi_infinite] <- exp(x[is_semi_infinite])
      x
    },
    model2rn = function(x) {
      x[is_semi_infinite] <- log(x[is_semi_infinite])
      x
    },
    ## Almost certainly this changes, will need to see with other
    ## model types though.
    deriv = function(x) {
      x[is_semi_infinite] <- exp(x[is_semi_infinite])
      x
    })
}
