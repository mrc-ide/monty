##' A `monty_sampler2` object can be passed into `monty_sample` in
##' order to draw samples from a distribution.  Samplers are stateful
##' objects that can mutate the state of a markov chain and advance
##' the MCMC one step.  Ordinarily users will not call this function,
##' but authors of samplers will call it from the constructor of their
##' sampler.
##'
##' See `vignette("writing-samplers")` for an introduction to writing
##' samplers.
##'
##' Control parameters are used to build the sampler.  These are
##' immutable after creation.  The format is unspecified by
##' `monty_sampler2` but typically this will be a named list.  The
##' sampler designer will construct this list and should take care not
##' to include anything mutable (e.g. environments) or hard to
##' serialise and transfer to another process here.
##'
##' @title Create a monty sampler
##'
##' @param name Name of the sampler. Usually this is the name of the
##'   algorithm and can include spaces and punctuation if desired.
##'
##' @param help Name of the function to direct users to find help.
##'   Usually that is the name of the constructor function.
##'
##' @param initialise A function to initialise the sampler.  This is
##'   called once at the start of the chain to set up any internal
##'   state, though in some cases it will not need to do very much.
##'   It must take arguments:
##'
##'   * `state_chain`: the state of the MCMC chain (TODO: describe)
##'   * `control`: the control parameters, as originally passed to
##'     `monty_sampler2`
##'   * `model`: the model being sampled from
##'   * the random number generator state, which the sampler may draw
##'     from.
##'
##'   Return `NULL` if your sampler is stateless, otherwise return an
##'   environment (e.g., created with `new.env(parent = emptyenv())`
##'   of state that will be updated at each iteration.  You can store
##'   whatever is convenient in this, for example a random walk
##'   sampler might store the eigendecomposition of a variance
##'   covariance matrix used in the proposal here.
##'
##' @param step The workhorse function of a sampler, propagating state
##'   (the pair (parameters, density)) forward one step in the chain.
##'   Typically, but not always, this will include a proposal,
##'   evaluation of a density, and an acceptance.
##'
##'   It must take arguments:
##'
##'   * `state_chain`: The state of the MCMC chain (as above)
##'
##'   * `state_sampler`: The state of the sampler, as passed back from
##'     `init`.  If your sampler is stateless this is `NULL`,
##'     otherwise it is an environment that you will modify by
##'     reference.
##'   * `control`: Sampler control parameters (as above)
##'   * `model`: The model (as above)
##'   * `rng`: The random number state, which you can use in the step
##'     (as above)
##'
##'   Return `state_chain`, updated after acceptance.
##'
##' @param state_dump Optionally, a function to prepare the chain
##'   state for serialisation.  If not given, we assume that
##'   `as.list()` is sufficient and use that (unless your state is
##'   `NULL`, in which case we use `identity`).  If provided then
##'   typically you will need to provide `state_restore`, too.
##'
##' @param state_restore Optionally, a function to take a dumped chain
##'   state and convert it back into an environment.  If not given, we
##'   assume that `list2env(x, parent = emptyenv())` is sufficient and
##'   use that (unless your state is `NULL`, in which case we use
##'   `identity`).  If provided then typically you will need to
##'   provide `state_dump`, too.
##'
##' @return A `monty_sampler2` object
##' @export
monty_sampler2 <- function(name, help, control, initialise, step,
                           state_dump = NULL, state_restore = NULL) {
  ## TODO: allow functions to be names and accept 'package' as an arg
  ## here, which will help with using the callr runner because we can
  ## organise loading packages and finding functions as required, even
  ## where the user has used a devtools-loaded package.
  assert_scalar_character(name)
  assert_scalar_character(help)
  ret <- list(
    name = name,
    help = help,
    control = control,
    initialise = initialise,
    step = step,
    state = list(dump = state_dump %||% monty_sampler2_default_dump,
                 restore = state_restore %||% monty_sampler2_default_restore))
  class(ret) <- "monty_sampler2"
  ret
}


monty_sampler2_default_dump <- function(x) {
  if (is.null(x)) x else as.list(x)
}


monty_sampler2_default_restore <- function(x) {
  if (is.null(x)) x else list2env(x, parent = emptyenv())
}



is_v2_sampler <- function(sampler) {
  inherits(sampler, "monty_sampler2")
}


##' @export
print.monty_sampler <- function(x, ...) {
  cli::cli_h1("<monty_sampler: {x$name} ({x$help})>")
  cli::cli_alert_info("Use {.help monty_sample} to use this sampler")
  cli::cli_alert_info("See {.help {x$help}} for more information")
  invisible(x)
}


monty_sampler_adaptive2 <- function(initial_vcv,
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
  ## TODO: validation generally, but especially that initial_vcv is ok
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
    forget_end = forget_end,
    adapt_end = adapt_end,
    pre_diminish = pre_diminish,
    boundaries = boundaries)

  monty_sampler2("Adaptive Metropolis-Hastings",
                 "monty_sampler_adaptive",
                 control,
                 msa2_initialise,
                 msa2_step,
                 msa2_state_dump,
                 msa2_state_restore)
}


msa2_initialise <- function(state_chain, control, model, rng) {
  require_deterministic(model,
                        "Can't use adaptive sampler with stochastic models")
  stopifnot(state_chain$n_chains == 1)

  pars <- state_chain$pars

  initial_vcv <- sampler_validate_vcv(control$initial_vcv, pars)

  msa2_sampler_state(control, pars, model, rng)
}


msa2_sampler_state <- function(control, pars, model, rng) {
  state <- new.env(parent = emptyenv())

  state$weight <- 0
  state$iteration <- 0
  state$mean <- unname(pars)

  n_pars <- length(pars)
  state$autocorrelation <- array(0, dim(control$initial_vcv))
  state$vcv <- update_vcv(state$mean, state$autocorrelation, state$weight)

  state$scaling <- control$initial_scaling
  state$scaling_weight <- control$initial_scaling_weight

  state$history_pars <- NULL
  state$included <- integer()
  state$scaling_history <- control$scaling

  state
}


msa2_step <- function(state_chain, state_sampler, control, model, rng) {
  vcv <- calc_proposal_vcv(state_sampler$scaling,
                           state_sampler$vcv,
                           state_sampler$weight,
                           control$initial_vcv,
                           control$initial_vcv_weight)

  proposal <-
    make_random_walk_proposal(vcv, model$domain, control$boundaries)
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
  state <- update_state(state, pars_next, density_next, accept, model, rng)

  update_adaptive2(state_sampler, control, state_chain$pars, accept_prob)

  state_chain
}


msa2_state_dump <- function(state_sampler) {
  as.list(state_sampler)
}


msa2_state_restore <- function(state_sampler) {
  ## We might want to also allow 'control' or 'model' here too
  list2env(state_sampler, parent = emptyenv())
}


update_adaptive2 <- function(state, control, pars, accept_prob) {
  state$iteration <- state$iteration + 1
  state$history_pars <- rbind(state$history_pars, pars)
  if (state$iteration > control$adapt_end) {
    state$scaling_history <- c(state$scaling_history, state$scaling)
    return(adaptive)
  }

  if (state$iteration > control$pre_diminish) {
    state$scaling_weight <- state$scaling_weight + 1
  }

  is_replacement <- check_replacement(state$iteration, control$forget_rate,
                                      control$forget_end)
  if (is_replacement) {
    pars_remove <- state$history_pars[state$included[1L], ]
    state$included <- c(state$included[-1L], state$iteration)
  } else {
    pars_remove <- NULL
    state$included <- c(state$included, state$iteration)
    state$weight <- state$weight + 1
  }

  ## TODO: we could simplify some of these functions now to take
  ## control as an argument
  state$scaling <-
    update_scaling(state$scaling, state$scaling_weight, accept_prob,
                   control$scaling_increment, control$min_scaling,
                   control$acceptance_target, control$log_scaling_update)
  state$scaling_history <- c(state$scaling_history, state$scaling)
  state$autocorrelation <- update_autocorrelation(
    pars, state$weight, state$autocorrelation, pars_remove)
  state$mean <- update_mean(pars, state$weight, state$mean, pars_remove)
  state$vcv <- update_vcv(state$mean, state$autocorrelation, state$weight)

  invisible(state)
}
