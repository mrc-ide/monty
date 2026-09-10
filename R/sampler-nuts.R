##' Create a No-U-Turn Sampler (NUTS) for gradient-based sampling.
##'
##' NUTS is an extension of Hamiltonian Monte Carlo (HMC) that avoids choosing
##' a fixed number of leapfrog integration steps. At each MCMC iteration, NUTS
##' builds a binary tree of forward/backward trajectory segments and stops when
##' the trajectory begins to turn back on itself (the U-turn criterion), a
##' divergence is detected (numerical instability in the leapfrog integrator,
##' seen as an implausibly large change in the Hamiltonian), or the tree
##' reaches `max_treedepth`.
##'
##' This is currently a single-chain sampler that requires a differentiable and
##' deterministic model. It uses the package RNG stream passed to the sampler
##' step and does not advertise simultaneous multi-chain execution.
##'
##' @title No-U-Turn Sampler
##'
##' @param epsilon Initial step size used by the leapfrog integrator. Must be
##'   strictly positive. If warmup adaptation is enabled, this is the starting
##'   value.
##'
##' @param max_treedepth Maximum binary-tree depth used when expanding a NUTS
##'   trajectory. This bounds the number of leapfrog steps taken per iteration
##'   to at most `2^max_treedepth`. Larger values allow longer trajectories
##'   before stopping, but increase computational cost per iteration.
##'
##' @param max_delta Maximum tolerated change in the Hamiltonian (energy)
##'   during tree expansion, used to detect divergent transitions.
##'   Trajectories that exceed this threshold are treated as divergent and
##'   stop expanding.
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
##' expansion is controlled by max_treedepth, and divergent trajectories are
##' detected using max_delta. During warmup, step size can be adapted towards
##' target_accept using dual averaging. After warmup, the adapted step size is
##' frozen and used for all remaining iterations.
##'
##' The number of iterations that hit `max_treedepth` and the number of
##' divergent transitions are tracked and exposed via `$details` on the
##' returned object (see [monty_sample]), mirroring the diagnostics reported
##' by other NUTS implementations such as Stan.
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
monty_sampler_nuts <- function(epsilon, max_treedepth = 10,
                               max_delta = 1000,
                               warmup_steps = 0L,
                               adapt_step_size = warmup_steps > 0,
                               target_accept = 0.8,
                               adapt_gamma = 0.05,
                               adapt_t0 = 10,
                               adapt_kappa = 0.75) {
  call <- environment()
  assert_scalar_positive_numeric(epsilon, allow_zero = FALSE)
  assert_scalar_size(max_treedepth, allow_zero = FALSE)
  assert_scalar_positive_numeric(max_delta, allow_zero = FALSE)
  assert_scalar_size(warmup_steps, allow_zero = TRUE)
  assert_scalar_logical(adapt_step_size)
  assert_scalar_numeric(target_accept)
  assert_scalar_positive_numeric(adapt_gamma, allow_zero = FALSE)
  assert_scalar_positive_numeric(adapt_t0, allow_zero = FALSE)
  assert_scalar_positive_numeric(adapt_kappa, allow_zero = FALSE)
  if (target_accept <= 0 || target_accept >= 1) {
    cli::cli_abort("'target_accept' must lie strictly between 0 and 1",
      arg = "target_accept", call = call
    )
  }
  if (adapt_kappa > 1) {
    cli::cli_abort("'adapt_kappa' must be no greater than 1",
      arg = "adapt_kappa", call = call
    )
  }

  control <- list(
    epsilon = epsilon,
    max_treedepth = max_treedepth,
    max_delta = max_delta,
    warmup_steps = warmup_steps,
    adapt_step_size = adapt_step_size && warmup_steps > 0,
    target_accept = target_accept,
    adapt_gamma = adapt_gamma,
    adapt_t0 = adapt_t0,
    adapt_kappa = adapt_kappa
  )

  properties <- monty_sampler_properties(
    allow_multiple_parameters = FALSE,
    requires_gradient = TRUE,
    requires_deterministic = TRUE
  )

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
    properties = properties
  )
}


sampler_nuts_initialise <- function(state_chain, control, model, rng) {
  state <- new.env(parent = emptyenv())
  state$iteration <- 0L
  state$H_bar <- 0
  state$log_epsilon <- log(control$epsilon)
  state$log_epsilon_bar <- log(control$epsilon)
  state$mu <- log(10 * control$epsilon)
  state$epsilon <- control$epsilon
  state$adapted <- FALSE
  state$n_divergent <- 0L
  state$n_max_treedepth_hit <- 0L
  state
}


sampler_nuts_step <- function(state_chain, state_sampler, control, model, rng) {
  hamiltonian <- function(theta_r) {
    sum(theta_r$r^2) / 2 - monty_model_density(model, theta_r$theta)
  }

  leapfrog <- function(current_theta_r, epsilon) {
    theta <- current_theta_r$theta
    r <- current_theta_r$r
    r <- r + epsilon * monty_model_gradient(model, theta) / 2
    theta <- theta + epsilon * r
    r <- r + epsilon * monty_model_gradient(model, theta) / 2
    list(theta = theta, r = r)
  }

  build_tree <- function(theta_r, u, v, j, epsilon, theta_r_0, delta) {
    if (j == 0) {
      theta_r_prop <- leapfrog(theta_r, v * epsilon)
      H_prop <- hamiltonian(theta_r_prop)
      H_0 <- hamiltonian(theta_r_0)
      n_prop <- as.integer(u <= exp(-H_prop))
      s_prop <- u < exp(delta - H_prop)
      list(
        minus = theta_r_prop,
        plus = theta_r_prop,
        theta_prop = theta_r_prop$theta,
        n_prop = n_prop,
        s_prop = s_prop,
        divergent = !s_prop,
        alpha = min(1, exp(H_0 - H_prop)),
        n_alpha = 1
      )
    } else {
      result_list <- build_tree(
        theta_r, u, v, j - 1, epsilon, theta_r_0, delta
      )
      if (isTRUE(result_list$s_prop)) {
        plus_or_minus <- if (v > 0) "plus" else "minus"
        alternative_list <- build_tree(
          result_list[[plus_or_minus]],
          u, v, j - 1, epsilon, theta_r_0, delta
        )
        result_list[[plus_or_minus]] <- alternative_list[[plus_or_minus]]

        sum_n_prop <- result_list$n_prop + alternative_list$n_prop
        if (sum_n_prop > 0) {
          if (monty_random_real(rng) < alternative_list$n_prop / sum_n_prop) {
            result_list$theta_prop <- alternative_list$theta_prop
          }
        }
        result_list$alpha <- result_list$alpha + alternative_list$alpha
        result_list$n_alpha <- result_list$n_alpha + alternative_list$n_alpha
        result_list$divergent <- result_list$divergent ||
          alternative_list$divergent
        result_list$s_prop <- alternative_list$s_prop &
          ((result_list$plus$theta - result_list$minus$theta) %*%
            result_list$minus$r >= 0) &
          ((result_list$plus$theta - result_list$minus$theta) %*%
            result_list$plus$r >= 0)
        result_list$n_prop <- sum_n_prop
      }
      result_list
    }
  }

  theta <- state_chain$pars
  theta_prop <- theta
  r0 <- drop(monty_random_n_normal(length(theta), 0, 1, rng))
  theta_r_0 <- list(theta = theta, r = r0)
  u <- monty_random_real(rng) * exp(-hamiltonian(theta_r_0))
  tree_list <- list(
    minus = theta_r_0,
    plus = theta_r_0
  )
  j <- 0L
  n <- 1L
  s <- TRUE
  divergent_transition <- FALSE
  epsilon_step <- state_sampler$epsilon

  while (s && j < control$max_treedepth) {
    v <- if (monty_random_real(rng) < 0.5) -1 else 1
    plus_or_minus <- if (v > 0) "plus" else "minus"
    tree_list <- build_tree(
      tree_list[[plus_or_minus]],
      u, v, j, epsilon_step, theta_r_0, control$max_delta
    )

    if (isTRUE(tree_list$s_prop)) {
      if (monty_random_real(rng) < min(1, tree_list$n_prop / n)) {
        theta_prop <- tree_list$theta_prop
      }
    }

    divergent_transition <- divergent_transition || isTRUE(tree_list$divergent)
    n <- n + tree_list$n_prop
    s <- isTRUE(tree_list$s_prop) &
      ((tree_list$plus$theta - tree_list$minus$theta) %*% tree_list$minus$r >= 0) &
      ((tree_list$plus$theta - tree_list$minus$theta) %*% tree_list$plus$r >= 0)
    j <- j + 1L
  }
  hit_max_treedepth <- isTRUE(s)

  state_sampler$n_divergent <-
    state_sampler$n_divergent + as.integer(divergent_transition)
  state_sampler$n_max_treedepth_hit <-
    state_sampler$n_max_treedepth_hit + as.integer(hit_max_treedepth)

  if (tree_list$n_alpha > 0) {
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
  list(
    iteration = state$iteration,
    H_bar = state$H_bar,
    log_epsilon = state$log_epsilon,
    log_epsilon_bar = state$log_epsilon_bar,
    mu = state$mu,
    epsilon = state$epsilon,
    adapted = state$adapted,
    n_divergent = state$n_divergent,
    n_max_treedepth_hit = state$n_max_treedepth_hit
  )
}


sampler_nuts_combine <- function(state, control) {
  ## NUTS currently only supports carrying forward a single chain's worth
  ## of adaptation and diagnostic state; when multiple chains are run we
  ## report state (including diagnostic counts) from the first chain only.
  ## This mirrors the existing single-chain scope of this sampler and
  ## should be revisited if multi-chain support is added.
  state[[1]]
}


sampler_nuts_restore <- function(chain_id, state_chain, state_sampler, control,
                                 model) {
  list2env(state_sampler, parent = emptyenv())
}


sampler_nuts_details <- function(state, control) {
  list(
    epsilon = state$epsilon,
    iteration = state$iteration,
    adapted = state$adapted,
    warmup_steps = control$warmup_steps,
    n_divergent = state$n_divergent,
    n_max_treedepth_hit = state$n_max_treedepth_hit
  )
}
