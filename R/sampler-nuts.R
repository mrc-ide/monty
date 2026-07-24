##' Create a No-U-Turn Sampler (NUTS) for gradient-based sampling.
##'
##' NUTS is an extension of Hamiltonian Monte Carlo (HMC) that avoids choosing
##' a fixed number of leapfrog integration steps. At each MCMC iteration, NUTS
##' builds a binary tree of forward/backward trajectory segments and stops when
##' the trajectory begins to turn back on itself (the U-turn criterion).
##'
##' This is currently a single-chain sampler that requires a differentiable and
##' deterministic model. It uses the package RNG stream passed to the sampler
##' step and does not advertise simultaneous multi-chain execution.
##'
##' @title No-U-Turn Sampler
##'
##' @param epsilon Initial step size used by the leapfrog integrator. If
##'   warmup adaptation is enabled, this is the starting value.
##'
##' @param max_treedepth Maximum binary-tree depth used when expanding a NUTS
##'   trajectory. Larger values allow longer trajectories before stopping, but
##'   increase computational cost per iteration.
##'
##' @param warmup_steps Number of warmup iterations used to adapt step size.
##'   Set to 0 (the default) to disable warmup adaptation.
##'
##' @param adapt_step_size Logical, indicating if step size should be adapted
##'   during warmup using dual averaging. Adaptation is only active when
##'   warmup_steps > 0.
##'
##' @param target_accept Target average acceptance probability used by
##'   dual-averaging adaptation. Must be in (0, 1). Larger values generally
##'   produce smaller step sizes.
##'
##' @param adapt_gamma Positive regularization scale used by dual averaging.
##'
##' @param adapt_t0 Positive adaptation offset used by dual averaging that
##'   stabilizes early warmup iterations.
##'
##' @param adapt_kappa Adaptation decay exponent in (0, 1] for the running
##'   dual-averaging average.
##'
##' @details
##' The proposal uses leapfrog integration and a standard Normal momentum. Tree
##' expansion is controlled by max_treedepth. During warmup, step size can be
##' adapted towards target_accept using dual averaging. After warmup, the
##' adapted step size is frozen and used for all remaining iterations.
##'
##' @references
##' Hoffman MD, Gelman A (2014). The No-U-Turn Sampler: Adaptively Setting
##' Path Lengths in Hamiltonian Monte Carlo. Journal of Machine Learning
##' Research 15(1):1593-1623.
##'
##' Nesterov Y (2009). Primal-dual subgradient methods for convex problems.
##' Mathematical Programming 120:221-259.
##'
##' @return A [monty_sampler] object that can be used with [monty_sample].
##'
##' @seealso [monty_sample], [monty_sampler_hmc]
##'
##' @export
monty_sampler_nuts <- function(epsilon, max_treedepth = 1000,
                               warmup_steps = 0L,
                               adapt_step_size = warmup_steps > 0,
                               target_accept = 0.8,
                               adapt_gamma = 0.05,
                               adapt_t0 = 10,
                               adapt_kappa = 0.75) {
  call <- environment()
  assert_scalar_numeric(epsilon)
  assert_scalar_size(max_treedepth, allow_zero = FALSE)
  assert_scalar_size(warmup_steps, allow_zero = TRUE)
  assert_scalar_logical(adapt_step_size)
  assert_scalar_numeric(target_accept)
  assert_scalar_positive_numeric(adapt_gamma, allow_zero = FALSE)
  assert_scalar_positive_numeric(adapt_t0, allow_zero = FALSE)
  assert_scalar_positive_numeric(adapt_kappa, allow_zero = FALSE)
  if (target_accept <= 0 || target_accept >= 1) {
    cli::cli_abort("'target_accept' must lie strictly between 0 and 1",
                   arg = "target_accept", call = call)
  }
  if (adapt_kappa > 1) {
    cli::cli_abort("'adapt_kappa' must be no greater than 1",
                   arg = "adapt_kappa", call = call)
  }

  control <- list(
    epsilon = epsilon,
    max_treedepth = max_treedepth,
    warmup_steps = warmup_steps,
    adapt_step_size = adapt_step_size && warmup_steps > 0,
    target_accept = target_accept,
    adapt_gamma = adapt_gamma,
    adapt_t0 = adapt_t0,
    adapt_kappa = adapt_kappa)

  properties <- monty_sampler_properties(
    allow_multiple_parameters = FALSE,
    requires_gradient = TRUE,
    requires_deterministic = TRUE)

  monty_sampler(
    "No-U-Turn Sampler",
    "monty_sampler_nuts",
    control,
    sampler_nuts_initialise,
    sampler_nuts_step,
    sampler_nuts_dump,
    sampler_nuts_combine,
    sampler_nuts_restore,
    sampler_nuts_details,
    properties = properties)
}


sampler_nuts_initialise <- function(state_chain, control, model, rng) {
  if (!control$adapt_step_size) {
    return(NULL)
  }
  state <- new.env(parent = emptyenv())
  state$iteration <- 0L
  state$H_bar <- 0
  state$log_epsilon <- log(control$epsilon)
  state$log_epsilon_bar <- log(control$epsilon)
  state$mu <- log(10 * control$epsilon)
  state$epsilon <- control$epsilon
  state$adapted <- FALSE
  state
}


sampler_nuts_step <- function(state_chain, state_sampler, control, model, rng) {
  hamiltonian <- function(theta, r) {
    sum(r^2) / 2 - monty_model_density(model, theta)
  }

  leapfrog <- function(current_theta, current_r, epsilon) {
    theta <- drop(current_theta)
    r <- drop(current_r)
    r <- drop(r + epsilon * monty_model_gradient(model, theta) / 2)
    theta <- drop(theta + epsilon * r)
    r <- drop(r + epsilon * monty_model_gradient(model, theta) / 2)
    list(theta = theta, r = r)
  }

  build_tree <- function(theta, r, u, v, j, epsilon, theta_0, r_0,
                         delta) {
    if (j == 0) {
      theta_r_prop <- leapfrog(theta, r, v * epsilon)
      H_prop <- hamiltonian(theta_r_prop$theta, theta_r_prop$r)
      H_0 <- hamiltonian(theta_0, r_0)
      n_prop <- as.integer(u <= exp(-H_prop))
      s_prop <- u < exp(delta - H_prop)
      list(
        theta_minus = theta_r_prop$theta,
        r_minus = theta_r_prop$r,
        theta_plus = theta_r_prop$theta,
        r_plus = theta_r_prop$r,
        theta_prop = theta_r_prop$theta,
        n_prop = n_prop,
        s_prop = s_prop,
        alpha = min(1, exp(H_0 - H_prop)),
        n_alpha = 1)
    } else {
      result_list <- build_tree(theta, r, u, v, j - 1, epsilon, theta_0,
                                r_0, delta)
      if (result_list$s_prop) {
        if (v == -1) {
          alternative_list <- build_tree(
            result_list$theta_minus, result_list$r_minus,
            u, v, j - 1, epsilon, theta_0, r_0, delta)
          result_list$theta_minus <- alternative_list$theta_minus
          result_list$r_minus <- alternative_list$r_minus
        } else {
          alternative_list <- build_tree(
            result_list$theta_plus, result_list$r_plus,
            u, v, j - 1, epsilon, theta_0, r_0, delta)
          result_list$theta_plus <- alternative_list$theta_plus
          result_list$r_plus <- alternative_list$r_plus
        }

        sum_n_prop <- result_list$n_prop + alternative_list$n_prop
        if (sum_n_prop > 0) {
          if (monty_random_real(rng) < alternative_list$n_prop / sum_n_prop) {
            result_list$theta_prop <- alternative_list$theta_prop
          }
        }
        result_list$alpha <- result_list$alpha + alternative_list$alpha
        result_list$n_alpha <- result_list$n_alpha + alternative_list$n_alpha
        result_list$s_prop <- alternative_list$s_prop &
          ((result_list$theta_plus - result_list$theta_minus) %*%
             result_list$r_minus >= 0) &
          ((result_list$theta_plus - result_list$theta_minus) %*%
             result_list$r_plus >= 0)
        result_list$n_prop <- sum_n_prop
      }
      result_list
    }
  }

  theta <- state_chain$pars
  theta_prop <- theta
  r0 <- drop(monty_random_n_normal(length(theta), 0, 1, rng))
  u <- monty_random_real(rng) * exp(-hamiltonian(theta, r0))
  tree_list <- list(
    theta_minus = theta,
    r_minus = r0,
    theta_plus = theta,
    r_plus = r0)
  j <- 0L
  n <- 1L
  s <- TRUE
  epsilon_step <- control$epsilon
  if (!is.null(state_sampler)) {
    epsilon_step <- state_sampler$epsilon
  }

  while (s) {
    v <- if (monty_random_real(rng) < 0.5) -1 else 1
    if (v == -1) {
      tree_list <- build_tree(
        tree_list$theta_minus, tree_list$r_minus,
        u, v, j, epsilon_step, theta, r0, control$max_treedepth)
    } else {
      tree_list <- build_tree(
        tree_list$theta_plus, tree_list$r_plus,
        u, v, j, epsilon_step, theta, r0, control$max_treedepth)
    }

    if (tree_list$s_prop) {
      if (monty_random_real(rng) < min(1, tree_list$n_prop / n)) {
        theta_prop <- tree_list$theta_prop
      }
    }

    n <- n + tree_list$n_prop
    s <- tree_list$s_prop &
      ((tree_list$theta_plus - tree_list$theta_minus) %*% tree_list$r_minus >= 0) &
      ((tree_list$theta_plus - tree_list$theta_minus) %*% tree_list$r_plus >= 0)
    j <- j + 1L
  }

  if (!is.null(state_sampler) && tree_list$n_alpha > 0) {
    accept_stat <- tree_list$alpha / tree_list$n_alpha
    if (!is.finite(accept_stat)) {
      accept_stat <- 0
    }
    accept_stat <- min(1, max(0, accept_stat))
    sampler_nuts_update_epsilon(state_sampler, control, accept_stat)
  }

  density_next <- monty_model_density(model, theta_prop)
  update_state(state_chain, theta_prop, density_next, TRUE, model)
}


sampler_nuts_update_epsilon <- function(state, control, accept_stat) {
  if (!control$adapt_step_size || state$adapted) {
    return(invisible(NULL))
  }

  state$iteration <- state$iteration + 1L
  m <- state$iteration

  eta <- 1 / (m + control$adapt_t0)
  state$H_bar <- (1 - eta) * state$H_bar +
    eta * (control$target_accept - accept_stat)

  state$log_epsilon <- state$mu -
    sqrt(m) / control$adapt_gamma * state$H_bar
  state$epsilon <- exp(state$log_epsilon)

  w <- m^(-control$adapt_kappa)
  state$log_epsilon_bar <- w * state$log_epsilon +
    (1 - w) * state$log_epsilon_bar

  if (m >= control$warmup_steps) {
    state$adapted <- TRUE
    state$epsilon <- exp(state$log_epsilon_bar)
    state$log_epsilon <- log(state$epsilon)
  }

  invisible(NULL)
}


sampler_nuts_dump <- function(state, control) {
  if (is.null(state)) {
    return(NULL)
  }
  list(iteration = state$iteration,
       H_bar = state$H_bar,
       log_epsilon = state$log_epsilon,
       log_epsilon_bar = state$log_epsilon_bar,
       mu = state$mu,
       epsilon = state$epsilon,
       adapted = state$adapted)
}


sampler_nuts_combine <- function(state, control) {
  if (all(vlapply(state, is.null))) {
    return(NULL)
  }
  state[[1]]
}


sampler_nuts_restore <- function(chain_id, state_chain, state_sampler, control,
                                 model) {
  if (is.null(state_sampler)) {
    return(NULL)
  }
  list2env(state_sampler, parent = emptyenv())
}


sampler_nuts_details <- function(state, control) {
  if (is.null(state)) {
    return(NULL)
  }
  list(epsilon = state$epsilon,
       iteration = state$iteration,
       adapted = state$adapted,
       warmup_steps = control$warmup_steps)
}