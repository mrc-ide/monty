##' Create an adaptive Metropolis-Hastings sampler, which will tune
##' its variance covariance matrix (vs the simple random walk
##' sampler [monty_sampler_random_walk]).
##'
##' Efficient exploration of the parameter space during an MCMC might
##' be difficult when the target distribution is of high
##' dimensionality, especially if the target probability distribution
##' present a high degree of correlation.  Adaptive schemes are used
##' to "learn" on the fly the correlation structure by updating the
##' proposal distribution by recalculating the empirical
##' variance-covariance matrix and rescale it at each adaptive step of
##' the MCMC.
##'
##' Our implementation of an adaptive MCMC algorithm is based on an
##' adaptation of the "accelerated shaping" algorithm in Spencer (2021).
##' The algorithm is based on a random-walk Metropolis-Hastings algorithm where
##' the proposal is a multi-variate Normal distribution centred on the current
##' point.
##'
##' Spencer SEF (2021) Accelerating adaptation in the adaptive
##' Metropolis–Hastings random walk algorithm. Australian & New Zealand Journal
##' of Statistics 63:468-484.
##'
##' @title Adaptive Metropolis-Hastings Sampler
##'
##' @param initial_vcv An initial variance covariance matrix; we'll start
##'   using this in the proposal, which will gradually become more weighted
##'   towards the empirical covariance matrix calculated from the chain.
##'
##' @param initial_vcv_weight Weight of the initial variance-covariance
##'   matrix used to build the proposal of the random-walk. Higher
##'   values translate into higher confidence of the initial
##'   variance-covariance matrix and means that update from additional
##'   samples will be slower.
##'
##' @param initial_scaling The initial scaling of the variance
##'   covariance matrix to be used to generate the multivariate normal
##'   proposal for the random-walk Metropolis-Hastings algorithm. To generate
##'   the proposal matrix, the weighted variance covariance matrix is
##'   multiplied by the scaling parameter squared times 2.38^2 / n_pars (where
##'   n_pars is the number of fitted parameters). Thus, in a Gaussian target
##'   parameter space, the optimal scaling will be around 1.
##'
##' @param initial_scaling_weight The initial weight used in the scaling update.
##'   The scaling weight will increase after the first `pre_diminish`
##'   iterations, and as the scaling weight increases the adaptation of the
##'   scaling diminishes. If `NULL` (the default) the value is
##'   5 / (acceptance_target * (1 - acceptance_target)).
##'
##' @param min_scaling The minimum scaling of the variance covariance
##'   matrix to be used to generate the multivariate normal proposal
##'   for the random-walk Metropolis-Hastings algorithm.
##'
##' @param scaling_increment The scaling increment which is added or
##'   subtracted to the scaling factor of the variance-covariance
##'   after each adaptive step. If `NULL` (the default) then an optimal
##'   value will be calculated.
##'
##' @param log_scaling_update Logical, whether or not changes to the
##'   scaling parameter are made on the log-scale.
##'
##' @param acceptance_target The target for the fraction of proposals
##'   that should be accepted (optimally) for the adaptive part of the
##'   chain.
##'
##' @param forget_rate The rate of forgetting early parameter sets from the
##'   empirical variance-covariance matrix in the MCMC chains. For example,
##'   `forget_rate = 0.2` (the default) means that once in every 5th iterations
##'   we remove the earliest parameter set included, so would remove the 1st
##'   parameter set on the 5th update, the 2nd on the 10th update, and so
##'   on. Setting `forget_rate = 0` means early parameter sets are never
##'   forgotten.
##'
##' @param forget_end The final iteration at which early parameter sets can
##'   be forgotten. Setting `forget_rate = Inf` (the default) means that the
##'   forgetting mechanism continues throughout the chains. Forgetting early
##'   parameter sets becomes less useful once the chains have settled into the
##'   posterior mode, so this parameter might be set as an estimate of how long
##'   that would take.
##'
##' @param adapt_end The final iteration at which we can adapt the multivariate
##'   normal proposal. Thereafter the empirical variance-covariance matrix, its
##'   scaling and its weight remain fixed. This allows the adaptation to be
##'   switched off at a certain point to help ensure convergence of the chain.
##'
##' @param pre_diminish The number of updates before adaptation of the scaling
##'   parameter starts to diminish. Setting `pre_diminish = 0` means there is
##'   diminishing adaptation of the scaling parameter from the offset, while
##'   `pre_diminish = Inf` would mean there is never diminishing adaptation.
##'   Diminishing adaptation should help the scaling parameter to converge
##'   better, but while the chains find the location and scale of the posterior
##'   mode it might be useful to explore with it switched off.
##'
##' @param boundaries Control the behaviour of proposals that are
##'   outside the model domain.  The supported options are:
##'
##'   * "reflect" (the default): we reflect proposed parameters that
##'     lie outside the domain back into the domain (as many times as
##'     needed)
##'
##'   * "reject": we do not evaluate the density function, and return
##'     `-Inf` for its density instead.
##'
##'   * "ignore": evaluate the point anyway, even if it lies outside
##'     the domain.
##'
##' The initial point selected will lie within the domain, as this is
##' enforced by [monty_sample].
##'
##' @return A `monty_sampler` object, which can be used with
##'   [monty_sample]
##'
##' @export
##' @examples
##' m <- monty_example("gaussian", matrix(c(1, 0.5, 0.5, 2), 2, 2))
##' vcv <- diag(2) * 0.1
##'
##' # Sampling with a random walk
##' s_rw <- monty_sampler_random_walk(vcv)
##' res_rw <- monty_sample(m, s_rw, 1000)
##'
##' s_adapt <- monty_sampler_adaptive(vcv)
##' res_adapt <- monty_sample(m, s_adapt, 1000)
##'
##' plot(drop(res_adapt$density), type = "l", col = 4)
##' lines(drop(res_rw$density), type = "l", col = 2)
##'
##' # Estimated vcv from the sampler at the end of the simulation
##' res_adapt$details$vcv[, , 1]
##'
##' @examplesIf requireNamespace("coda")
##' coda::effectiveSize(coda::as.mcmc.list(res_rw))
##' coda::effectiveSize(coda::as.mcmc.list(res_adapt))
monty_sampler_adaptive <- function(initial_vcv,
                                   initial_vcv_weight = 1000,
                                   initial_scaling = 1,
                                   initial_scaling_weight = NULL,
                                   min_scaling = 0,
                                   scaling_increment = NULL,
                                   log_scaling_update = TRUE,
                                   acceptance_target = 0.234,
                                   forget_rate = 0.2,
                                   forget_end = Inf,
                                   adapt_end = Inf,
                                   pre_diminish = 0,
                                   boundaries = "reflect") {
  check_vcv(initial_vcv, allow_3d = TRUE, call = environment())
  ## Convert to a 3d-array so that we never have to special case
  ## either the single or multiparameter access:
  if (length(dim(initial_vcv)) == 2) {
    initial_vcv <- array(initial_vcv, c(dim(initial_vcv), 1))
  }

  forget_rate_inverse <- validate_forget_rate(forget_rate)

  if (is.null(scaling_increment)) {
    n_pars <- nrow(initial_vcv)
    scaling_increment <- calc_scaling_increment(
      n_pars, acceptance_target, log_scaling_update)
  }

  if (is.null(initial_scaling_weight)) {
    initial_scaling_weight <- 5 / (acceptance_target * (1 - acceptance_target))
  }

  control <- list(
    initial_vcv = initial_vcv,
    initial_vcv_weight = initial_vcv_weight,
    initial_scaling = initial_scaling,
    initial_scaling_weight = initial_scaling_weight,
    min_scaling = min_scaling,
    scaling_increment = scaling_increment,
    log_scaling_update = log_scaling_update,
    acceptance_target = acceptance_target,
    forget_rate = forget_rate,
    forget_rate_inverse = forget_rate_inverse,
    forget_end = forget_end,
    adapt_end = adapt_end,
    pre_diminish = pre_diminish,
    boundaries = boundaries)

  monty_sampler("Adaptive Metropolis-Hastings",
                "monty_sampler_adaptive",
                control,
                sampler_random_walk_adaptive_initialise,
                sampler_random_walk_adaptive_step,
                sampler_random_walk_adaptive_state_dump,
                sampler_random_walk_adaptive_state_combine,
                sampler_random_walk_adaptive_state_restore,
                sampler_random_walk_adaptive_details)
}


sampler_random_walk_adaptive_initialise <- function(state_chain, control,
                                                    model, rng) {
  require_deterministic(model,
                        "Can't use adaptive sampler with stochastic models")

  pars <- state_chain$pars
  multiple_parameters <- length(dim2(pars)) > 1

  sampler_validate_vcv(control$initial_vcv, pars)

  if (multiple_parameters) {
    n_pars <- nrow(pars)
    n_sets <- ncol(pars)
  } else {
    n_pars <- length(pars)
    n_sets <- 1L
  }

  state <- new.env(parent = emptyenv())

  ## Iteration and weight advance the same for all chains
  state$iteration <- 0L
  state$weight <- 0L

  ## Everything else varies by the number of parameter sets:
  state$mean <- matrix(unname(pars), n_pars, n_sets)

  state$autocorrelation <- array(0, c(n_pars, n_pars, n_sets))
  state$vcv <- update_vcv(state$mean, state$autocorrelation, state$weight)

  state$scaling <- rep(control$initial_scaling, n_sets)
  state$scaling_weight <- rep(control$initial_scaling_weight, n_sets)

  state$history_pars <- numeric(0)
  state$scaling_history <- rep(control$initial_scaling, n_sets)

  state
}


sampler_random_walk_adaptive_step <- function(state_chain, state_sampler,
                                              control, model, rng) {
  vcv <- calc_proposal_vcv(state_sampler$scaling,
                           state_sampler$vcv,
                           state_sampler$weight,
                           control$initial_vcv,
                           control$initial_vcv_weight)

  proposal <-
    make_random_walk_proposal_fn(vcv, model$domain, control$boundaries)
  pars_next <- proposal(state_chain$pars, rng)

  u <- monty_random_real(rng)
  reject_some <- control$boundaries == "reject" &&
    !all(i <- is_parameters_in_domain(pars_next, model$domain))
  if (reject_some) {
    density_next <- rep(-Inf, length(state_chain$density))
    if (any(i)) {
      density_next[i] <- model$density(pars_next[, i, drop = FALSE])
    }
  } else {
    density_next <- model$density(pars_next)
  }

  accept_prob <- pmin(1, exp(density_next - state_chain$density))

  accept <- u < accept_prob
  state_chain <- update_state(
    state_chain, pars_next, density_next, accept, model, rng)

  update_adaptive(state_sampler, control, state_chain$pars, accept_prob)

  state_chain
}


sampler_random_walk_adaptive_details <- function(state, control) {
  ## Dropping these, as we had previously decided to -- we could
  ## return them too but no real need.
  state[setdiff(names(state), c("history_pars", "scaling"))]
}


## State here is the *sampler* state
update_adaptive <- function(state, control, pars, accept_prob) {
  state$iteration <- state$iteration + 1

  if (state$iteration > control$adapt_end) {
    state$scaling_history <- c(state$scaling_history, state$scaling)
    return(state)
  }

  if (state$iteration > control$pre_diminish) {
    state$scaling_weight <- state$scaling_weight + 1
  }

  if (control$forget_rate > 0) {
    state$history_pars <- c(state$history_pars, pars)
  }
  replace <- check_replacement(state$iteration, control)
  if (replace > 0) {
    ## We store our history in a big flat vector, with each block of
    ## (len_pars = n_pars * n_pars) being a set of parameters from an
    ## iteration past.  We can index into these easily enough by
    ## offsetting by the number of previous iterations, then reshaping
    ## to match our current pars.
    len_pars <- length(pars)
    i <- seq_len(len_pars) + ((replace - 1) * len_pars)
    pars_remove <- state$history_pars[i]
    dim(pars_remove) <- dim(pars)
  } else {
    pars_remove <- NULL
    state$weight <- state$weight + 1L
  }

  state$scaling <-
    update_scaling(state$scaling, state$scaling_weight, accept_prob,
                   control)
  state$scaling_history <- c(state$scaling_history, state$scaling)
  state$autocorrelation <- update_autocorrelation(
    pars, state$weight, state$autocorrelation, pars_remove)
  state$mean <- update_mean(pars, state$weight, state$mean, pars_remove)
  state$vcv <- update_vcv(state$mean, state$autocorrelation, state$weight)

  state
}


calc_scaling_increment <- function(n_pars, acceptance_target,
                                   log_scaling_update) {
  if (log_scaling_update) {
    A <- -stats::qnorm(acceptance_target / 2) # nolint

    scaling_increment <-
      (1 - 1 / n_pars) * (sqrt(2 * pi) * exp(A^2 / 2)) / (2 * A) +
      1 / (n_pars * acceptance_target * (1 - acceptance_target))
  } else {
    scaling_increment <- 1 / 100
  }

  scaling_increment
}


qp <- function(x) {
  if (is.matrix(x)) {
    dx <- dim(x)
    if (dx[[2]] == 1) {
      ox <- outer(x, x)
    } else {
      ox <- apply(x, 2, function(xi) outer(xi, xi))
    }
    array(ox, dx[c(1, 1, 2)])
  } else {
    n <- length(x)
    array(outer(x, x), c(n, n, 1))
  }
}


calc_proposal_vcv <- function(scaling, vcv, weight, initial_vcv,
                              initial_vcv_weight) {
  n_pars <- nrow(vcv)
  n_sets <- dim(vcv)[[3]]

  if (n_sets == 1) {
    ret <-
      ((weight - 1) * vcv +
         (initial_vcv_weight + n_pars + 1) * initial_vcv) /
      (weight + initial_vcv_weight + n_pars + 1)
    ret <- 2.38^2 / n_pars * scaling^2 * ret
  } else {
    ret <- vcv
    shared_initial_vcv <- dim(initial_vcv)[[3]] == 1

    ## There are a few ways of coping with the multiparameter case.
    ## One would be to replicate some of the scalars by n_pars^2 so
    ## that the arithmetic works out, another is this repetitive loop
    ## here.  It should be "fairly easy" to adjust this as needed if
    ## this ends up being slow.
    for (i in seq_len(n_sets)) {
      j <- if (shared_initial_vcv) 1L else i
      ret[, , i] <-
        ((weight - 1) * vcv[, , i] +
           (initial_vcv_weight + n_pars + 1) * initial_vcv[, , j]) /
        (weight + initial_vcv_weight + n_pars + 1)
      ret[, , i] <- 2.38^2 / n_pars * scaling[[i]]^2 * ret[, , i]
    }
  }
  ret
}


## If this iteration is due to be forgetten, return the index of the
## element to forget, otherwise return 0 to indicate that nothing will
## be forgotten.
check_replacement <- function(iteration, control) {
  is_replacement <-
    iteration <= control$forget_end &&
    iteration %% control$forget_rate_inverse == 0
  if (is_replacement) iteration %/% control$forget_rate_inverse else 0L
}


update_scaling <- function(scaling, scaling_weight, accept_prob, control) {
  scaling_change <- control$scaling_increment *
    (accept_prob - control$acceptance_target) /
    sqrt(scaling_weight)

  if (control$log_scaling_update) {
    pmax(control$min_scaling, scaling * exp(scaling_change))
  } else {
    pmax(control$min_scaling, scaling + scaling_change)
  }
}


update_autocorrelation <- function(pars, weight, autocorrelation, pars_remove) {
  if (is.null(pars_remove)) {
    if (weight > 2) {
      autocorrelation <- (1 - 1 / (weight - 1)) * autocorrelation +
        1 / (weight - 1) * qp(pars)
    } else {
      autocorrelation <- autocorrelation + qp(pars)
    }
  } else {
    qp_pars_diff <- qp(pars) - qp(pars_remove)
    if (weight > 2) {
      autocorrelation <- autocorrelation + 1 / (weight - 1) * qp_pars_diff
    } else {
      autocorrelation <- autocorrelation + qp_pars_diff
    }
  }

  autocorrelation
}


update_mean <- function(pars, weight, mean, pars_remove) {
  if (is.null(pars_remove)) {
    (1 - 1 / weight) * mean + 1 / weight * pars
  } else {
    mean + 1 / weight * (pars - pars_remove)
  }
}


update_vcv <- function(mean, autocorrelation, weight) {
  if (weight > 1) {
    autocorrelation - weight / (weight - 1) * qp(mean)
  } else {
    # array of 0's the same shape as the input:
    0 * autocorrelation
  }
}


validate_forget_rate <- function(forget_rate, call = parent.frame()) {
  if (forget_rate == 0) {
    return(Inf)
  }
  if (forget_rate < 0) {
    cli::cli_abort("Expected 'forget_rate' to be positive",
                   arg = "forget_rate", call = call)
  }
  if (forget_rate >= 1) {
    cli::cli_abort("Expected 'forget_rate' to be less than 1",
                   arg = "forget_rate", call = call)
  }
  ret <- 1 / forget_rate
  if (!rlang::is_integerish(ret)) {
    cli::cli_abort(
      "Expected 'forget_rate' to be the inverse of an integer",
      arg = "forget_rate", call = call)
  }
  as.integer(ret)
}


sampler_random_walk_adaptive_state_dump <- function(state, control) {
  ret <- as.list(state, sorted = TRUE)

  ## 'history_pars' is stored in a flat vector for easy accumulation,
  ## but here we reshape to something actually useful.  A bit of work
  ## in the simultaneous case to get the order <step> x <chain>, as we
  ## accumulate in the other order (all chains for a given step) and
  ## need to transpose here.  For the single case it's a unit
  ## dimension so it doesn't really matter.
  n_pars <- nrow(state$mean)
  n_sets <- ncol(state$mean)
  n_steps <- state$iteration
  if (length(ret$history_pars) == 0) {
    ret$history_pars <- array(0, c(n_pars, 0, n_sets))
  } else if (n_sets == 1) {
    ret$history_pars <- array(ret$history_pars, c(n_pars, n_steps, n_sets))
  } else {
    arr <- array(ret$history_pars, c(n_pars, n_sets, n_steps))
    ret$history_pars <- aperm(arr, c(1, 3, 2))
  }

  if (n_sets > 1) {
    ret$scaling_history <- matrix(ret$scaling_history,
                                  ncol = n_sets, byrow = TRUE)
    ret$iteration <- rep(ret$iteration, n_sets)
    ret$weight <- rep(ret$weight, n_sets)
  }

  ret
}


sampler_random_walk_adaptive_state_restore <- function(chain_id, state_chain,
                                                       state_sampler, control,
                                                       model) {
  if (length(chain_id) > 1) {
    state <- state_sampler
    state$iteration <- state$iteration[[1]]
    state$weight <- state$weight[[1]]
    state$scaling_history <- as.vector(t(state$scaling_history))
    state$history_pars <- as.vector(aperm(state$history_pars, c(1, 3, 2)))
  } else {
    state <- lapply(state_sampler, array_select_last, chain_id)
    state$history_pars <- as.vector(state$history_pars)
  }
  list2env(state, parent = emptyenv())
}


sampler_random_walk_adaptive_state_combine <- function(state, control) {
  ## The dimension order is always <pars>, <step>, <chain> so when
  ## combining results for single chain we have:
  ##
  ## autocorrelation: <pars> x <pars> x 1 -> <pars> x <pars> x <chain>
  ## history_pars: <pars> x <step> x 1 -> <pars> x <step> x <chain>
  ## iteration: 1 -> <chain>
  ## mean: <pars> x 1 -> <pars> x <chain>
  ## scaling: 1 -> <chain>
  ## scaling_history: <step> -> <step> x <chain>
  ## scaling_weight: 1 -> <chain>
  ## vcv: <pars> x <pars> x 1 -> <pars> x <pars> x <chain>
  ## weight: 1 -> <chain>
  join <- function(name, ...) {
    array_bind(arrays = lapply(state, "[[", name), ...)
  }

  ## We can probably express this purely in data, which might be
  ## useful for eventual generalisation, though I think I now have
  ## them all being simply "bind on the last element"
  list(autocorrelation = join("autocorrelation", on = 3),
       history_pars = join("history_pars", on = 3),
       iteration = join("iteration", on = 1),
       mean = join("mean", on = 2),
       scaling = join("scaling", on = 1),
       scaling_history = join("scaling_history", on = 2),
       scaling_weight = join("scaling_weight", on = 1),
       vcv = join("vcv", on = 3),
       weight = join("weight", on = 1))
}
