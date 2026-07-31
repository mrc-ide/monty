reference_nuts <- function() {
  ## This is a duplicate of the implementation in R/sampler-nuts.R, kept
  ## structurally in sync so that regression tests can compare production
  ## output against a known-working snapshot. It is not an independent
  ## verification of correctness (see tests that use pathological models
  ## directly for that).
  hamiltonian <- function(theta, r, model) {
    sum(r^2) / 2 - monty_model_density(model, theta)
  }

  leapfrog <- function(model, current_theta, current_r, epsilon) {
    theta <- drop(current_theta)
    r <- drop(current_r)
    r <- drop(r + epsilon * monty_model_gradient(model, theta) / 2)
    theta <- drop(theta + epsilon * r)
    r <- drop(r + epsilon * monty_model_gradient(model, theta) / 2)
    list(theta = theta, r = r)
  }

  build_tree <- function(theta, r, u, v, j, epsilon, theta_0, r_0, model, rng,
                         delta) {
    if (j == 0) {
      theta_r_prop <- leapfrog(model, theta, r, v * epsilon)
      H_prop <- hamiltonian(theta_r_prop$theta, theta_r_prop$r, model)
      H_0 <- hamiltonian(theta_0, r_0, model)
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
        divergent = !s_prop,
        alpha = min(1, exp(H_0 - H_prop)),
        n_alpha = 1)
    } else {
      result_list <- build_tree(theta, r, u, v, j - 1, epsilon, theta_0, r_0,
                                model, rng, delta)
      if (isTRUE(result_list$s_prop)) {
        if (v == -1) {
          alternative_list <- build_tree(
            result_list$theta_minus, result_list$r_minus,
            u, v, j - 1, epsilon, theta_0, r_0, model, rng, delta)
          result_list$theta_minus <- alternative_list$theta_minus
          result_list$r_minus <- alternative_list$r_minus
        } else {
          alternative_list <- build_tree(
            result_list$theta_plus, result_list$r_plus,
            u, v, j - 1, epsilon, theta_0, r_0, model, rng, delta)
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
        result_list$divergent <- result_list$divergent ||
          alternative_list$divergent
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

  NUTS_sampler_initialise <- function(state_chain, control, model, rng) {
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

  NUTS_sampler_step <- function(state_chain, state_sampler, control, model,
                                rng) {
    theta <- state_chain$pars
    theta_prop <- theta
    r0 <- drop(monty_random_n_normal(length(theta), 0, 1, rng))
    u <- monty_random_real(rng) * exp(-hamiltonian(theta, r0, model))
    tree_list <- list(
      theta_minus = theta,
      r_minus = r0,
      theta_plus = theta,
      r_plus = r0)
    j <- 0L
    n <- 1L
    s <- TRUE
    divergent_transition <- FALSE
    epsilon_step <- state_sampler$epsilon

    while (s && j < control$max_treedepth) {
      v <- if (monty_random_real(rng) < 0.5) -1 else 1
      if (v == -1) {
        tree_list <- build_tree(
          tree_list$theta_minus, tree_list$r_minus,
          u, v, j, epsilon_step, theta, r0, model, rng, control$max_delta)
      } else {
        tree_list <- build_tree(
          tree_list$theta_plus, tree_list$r_plus,
          u, v, j, epsilon_step, theta, r0, model, rng, control$max_delta)
      }

      if (isTRUE(tree_list$s_prop)) {
        if (monty_random_real(rng) < min(1, tree_list$n_prop / n)) {
          theta_prop <- tree_list$theta_prop
        }
      }

      divergent_transition <- divergent_transition || isTRUE(tree_list$divergent)
      n <- n + tree_list$n_prop
      s <- isTRUE(tree_list$s_prop) &
        ((tree_list$theta_plus - tree_list$theta_minus) %*% tree_list$r_minus >= 0) &
        ((tree_list$theta_plus - tree_list$theta_minus) %*% tree_list$r_plus >= 0)
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
      NUTS_sampler_update_epsilon(state_sampler, control, accept_stat)
    }

    density_next <- monty_model_density(model, theta_prop)
    update_state(state_chain, theta_prop, density_next, TRUE, model)
  }

  NUTS_sampler_update_epsilon <- function(state, control, accept_stat) {
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

  NUTS_sampler_dump <- function(state, control) {
    list(iteration = state$iteration,
         H_bar = state$H_bar,
         log_epsilon = state$log_epsilon,
         log_epsilon_bar = state$log_epsilon_bar,
         mu = state$mu,
         epsilon = state$epsilon,
         adapted = state$adapted,
         n_divergent = state$n_divergent,
         n_max_treedepth_hit = state$n_max_treedepth_hit)
  }

  NUTS_sampler_combine <- function(state, control) {
    state[[1]]
  }

  NUTS_sampler_restore <- function(chain_id, state_chain, state_sampler,
                                   control, model) {
    list2env(state_sampler, parent = emptyenv())
  }

  NUTS_sampler_details <- function(state, control) {
    list(epsilon = state$epsilon,
         iteration = state$iteration,
         adapted = state$adapted,
         warmup_steps = control$warmup_steps,
         n_divergent = state$n_divergent,
         n_max_treedepth_hit = state$n_max_treedepth_hit)
  }

  function(epsilon, max_treedepth = 10, max_delta = 1000,
          warmup_steps = 0L,
          adapt_step_size = warmup_steps > 0,
          target_accept = 0.8,
          adapt_gamma = 0.05,
          adapt_t0 = 10,
          adapt_kappa = 0.75) {
    control <- list(
      epsilon = epsilon,
      max_treedepth = max_treedepth,
      max_delta = max_delta,
      warmup_steps = warmup_steps,
      adapt_step_size = adapt_step_size && warmup_steps > 0,
      target_accept = target_accept,
      adapt_gamma = adapt_gamma,
      adapt_t0 = adapt_t0,
      adapt_kappa = adapt_kappa)
    monty_sampler(
      "NUTS Sampler",
      "NUTS sampler based on the original NUTS paper",
      control,
      NUTS_sampler_initialise,
      NUTS_sampler_step,
      NUTS_sampler_dump,
      NUTS_sampler_combine,
      NUTS_sampler_restore,
      NUTS_sampler_details)
  }
}
