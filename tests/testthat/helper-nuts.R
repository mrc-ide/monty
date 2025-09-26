reference_nuts <- function() {
  hamiltonian <- function(theta, r, model){
    sum(r^2) / 2  - monty_model_density(model, theta)
  }

  # perform 1 leafrog integration of step epsilon
  leapfrog <- function(model, current_theta, current_r, epsilon){
    # initialise to the current value of theta and r
    theta <- current_theta
    r <- current_r
    # Make a half step for momentum
    r <- r + epsilon * monty_model_gradient (model, theta) / 2
    # Make a full step for theta
    theta <- theta + epsilon * r
    # Make a half step for momentum
    r <- r + epsilon * monty_model_gradient(model, theta) / 2
    return(list(theta = theta, r = r))
  }

  build_tree <- function(theta, r, u, v, j, epsilon, theta_0, r_0, model, rng, delta = 1000)
  {
    if(j==0) {
      # base case, one leapfrog in direction v
      theta_r_prop <- leapfrog(model, theta, r, v*epsilon)
      H_prop <- hamiltonian(theta_r_prop$theta, theta_r_prop$r, model)
      H_0 <- hamiltonian(theta_0, r_0, model)
      n <- as.integer(u <= exp(-H_prop))
      s <- u < exp(delta - H_prop)
      return(list(theta_minus = theta_r_prop$theta,
                  r_minus = theta_r_prop$r,
                  theta_plus = theta_r_prop$theta,
                  r_plus = theta_r_prop$r,
                  theta_prop = theta_r_prop$theta,
                  n_prop = n,
                  s_prop = s,
                  alpha = min(1, exp(H_0-H_prop)),
                  n_alpha = 1)
             )
    } else { #j>0
      result_list <- build_tree(theta, r, u, v, j-1, epsilon, theta_0, r_0, model, rng, delta)
      if(result_list$s_prop){ # continue the tree unless stop condition is reached
        if(v==-1){
          alternative_list <- build_tree(result_list$theta_minus, result_list$r_minus,
                                         u, v, j-1, epsilon, theta_0, r_0, model, rng, delta)
          result_list$theta_minus <- alternative_list$theta_minus
          result_list$r_minus <- alternative_list$r_minus
        } else { #v==1
          alternative_list <- build_tree(result_list$theta_plus, result_list$r_plus,
                                         u, v, j-1, epsilon, theta_0, r_0, model, rng, delta)
          result_list$theta_plus <- alternative_list$theta_plus
          result_list$r_plus <- alternative_list$r_plus
        }
        sum_n_prop <- result_list$n_prop+alternative_list$n_prop
        if(sum_n_prop > 0)
          if(monty_random_real(rng)<alternative_list$n_prop/sum_n_prop)
            result_list$theta_prop <- alternative_list$theta_prop
        result_list$alpha <- result_list$alpha + alternative_list$alpha
        result_list$n_alpha <- result_list$n_alpha + alternative_list$n_alpha
        result_list$s_prop <- alternative_list$s_prop &
          ((result_list$theta_plus-result_list$theta_minus)%*%result_list$r_minus >= 0) &
          ((result_list$theta_plus-result_list$theta_minus)%*%result_list$r_plus >= 0)
        result_list$n_prop <- sum_n_prop
      }
      return(result_list)
    }
  }

  NUTS_sampler_initialise <- function(state_chain, control, model, rng) {
    NULL
  }

  NUTS_sampler_step <- function(state_chain, state_sampler, control, model, rng) {

    theta <- state_chain$pars
    theta_prop <- theta
    r0 <- monty_random_n_normal(length(theta), 0, 1, rng)
    u <- monty_random_real(rng)*exp(-hamiltonian(theta, r0, model))
    tree_list <- list(theta_minus = theta,
                      r_minus = r0,
                      theta_plus = theta,
                      r_plus = r0
                      )
    j <- 0
    n <- 1
    s <- TRUE
    while(s){
      v <- if (monty_random_real(rng) < 0.5) -1 else 1
      if(v==-1){
        tree_list <- build_tree(tree_list$theta_minus, tree_list$r_minus,
                                u, v, j, control$epsilon, theta, r0, model, rng, control$D_max)
      } else {
        tree_list <- build_tree(tree_list$theta_plus, tree_list$r_plus,
                                u, v, j, control$epsilon, theta, r0, model, rng, control$D_max)
      }
      if(tree_list$s_prop)
        if(monty_random_real(rng)<min(1,tree_list$n_prop/n))
          theta_prop <- tree_list$theta_prop
      n <- n + tree_list$n_prop
      s <- tree_list$s_prop &
        ((tree_list$theta_plus-tree_list$theta_minus)%*%tree_list$r_minus >= 0) &
        ((tree_list$theta_plus-tree_list$theta_minus)%*%tree_list$r_plus >= 0)
      j <- j+1
    }
    list(theta_prop=theta_prop, j=j, s=s, n=n, theta_minus=tree_list$theta_minus, theta_plus=tree_list$theta_plus)

    state_chain$pars <- theta_prop
    state_chain$density <- monty_model_density(model, theta_prop)
    state_chain
  }

  function(epsilon, D_max = 1000) {
    control <- list(epsilon = epsilon,
                    D_max = D_max)
    monty_sampler(
      "NUTS Sampler",
      "NUTS sampler based on the original NUTS paper",
      control,
      NUTS_sampler_initialise,
      NUTS_sampler_step)
  }
}
