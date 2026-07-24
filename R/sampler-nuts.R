##' Create a No-U-Turn Sampler (NUTS) for gradient-based sampling.
##'
##' This is a single-chain sampler that requires a differentiable and
##' deterministic model. It uses the package RNG stream passed to the
##' sampler step and does not currently advertise simultaneous multi-chain
##' execution.
##'
##' @title No-U-Turn Sampler
##'
##' @param epsilon Step size used by the leapfrog integrator.
##'
##' @param max_treedepth Maximum tree depth used while building the tree.
##'
##' @return A [monty_sampler] object that can be used with [monty_sample].
##'
##' @export
monty_sampler_nuts <- function(epsilon, max_treedepth = 1000) {
  assert_scalar_numeric(epsilon)
  assert_scalar_size(max_treedepth, allow_zero = FALSE)

  control <- list(
    epsilon = epsilon,
    max_treedepth = max_treedepth)

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
    properties = properties)
}


sampler_nuts_initialise <- function(state_chain, control, model, rng) {
  NULL
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

  while (s) {
    v <- if (monty_random_real(rng) < 0.5) -1 else 1
    if (v == -1) {
      tree_list <- build_tree(
        tree_list$theta_minus, tree_list$r_minus,
        u, v, j, control$epsilon, theta, r0, control$max_treedepth)
    } else {
      tree_list <- build_tree(
        tree_list$theta_plus, tree_list$r_plus,
        u, v, j, control$epsilon, theta, r0, control$max_treedepth)
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

  density_next <- monty_model_density(model, theta_prop)
  update_state(state_chain, theta_prop, density_next, TRUE, model)
}