##' Run chains *simultaneously*.  This differs from
##' [monty_runner_parallel], which runs chains individually in
##' parallel by working with models that can evaluate multiple
##' densities at the same time.  There are situations where this might
##' be faster than running in parallel, but primarily this exists so
##' that we can see that samplers can work with multiple samples at
##' once.
##'
##' @title Run MCMC chains simultaneously
##'
##' @inheritParams monty_runner_serial
##'
##' @return A runner of class `monty_runner` that can be passed to
##'   [monty_sample()]
##'
##' @export
##' @examples
##' m <- monty_example("banana")
##' s <- monty_sampler_random_walk(vcv = diag(2) * 0.01)
##' r <- monty_runner_simultaneous()
##' samples <- monty_sample(m, s, 200, runner = r)
monty_runner_simultaneous <- function(progress = NULL) {
  validate_suitable <- function(model) {
    require_multiple_parameters(
      model,
      "monty_runner_simultaneous requires support for multiple parameters",
      call = environment())
    ## These will both be relaxed later.
    require_deterministic(
      model,
      "Can't yet use multiple parameter sets with stochastic model",
      call = environment())
  }

  run <- function(pars, model, sampler, steps, rng) {
    validate_suitable(model)
    n_chains <- length(rng)
    pb <- progress_bar(n_chains, steps$total, progress, show_overall = FALSE)
    progress <- pb(seq_len(n_chains))
    rng_state <- lapply(rng, function(r) r$state())
    ## TODO: get the rng state back into 'rng' here, or (better) look
    ## at if we should just be using seed instead here perhaps?
    ## > rng_state <- matrix(res$internal$state$rng, ncol = n_chains)
    ## > for (i in seq_len(n_chains)) {
    ## >   rng[[i]]$set_state(rng_state[, i]) # not supported!
    ## > }
    monty_run_chains_simultaneous(pars, model, sampler,
                                  steps, progress, rng_state)
  }

  continue <- function(state, model, sampler, steps) {
    validate_suitable(model)
    n_chains <- length(state)
    pb <- progress_bar(n_chains, steps$total, progress, show_overall = FALSE)
    progress <- pb(seq_len(n_chains))
    monty_continue_chains_simultaneous(state, model, sampler,
                                       steps, progress)
  }

  monty_runner("Simultaneous",
               "monty_runner_simultaneous",
               run,
               continue)
}


## There is a lot of overlap here with monty_run_chain, we might
## later seek to harmonise this.  However, this is really just
## intended for developing and debugging the multiple-sample versions
## of the samplers, so it does not need to be particularly lovely.
##
## * we take our nice array format, break it into lists, and then
##   reform into essentially the same array format (later).  This is
##   hard to avoid.
## * there's quite a lot of churn around rng state
monty_run_chains_simultaneous <- function(pars, model, sampler,
                                          steps, progress, rng_state) {
  r_rng_state <- get_r_rng_state()
  n_chains <- length(rng_state)
  rng <- monty_rng$new(unlist(rng_state), n_chains)

  chain_state <- sampler$initialise(pars, model, rng)

  monty_run_chains_simultaneous2(chain_state, model, sampler,
                                 steps, progress, rng, r_rng_state)
}


monty_continue_chains_simultaneous <- function(state, model, sampler,
                                               steps, progress) {
  r_rng_state <- get_r_rng_state()
  n_chains <- length(state)
  n_pars <- length(model$parameters)

  ## NOTE this duplicates code in monty_run_chains_simultaneous; we
  ## could move this elsewhere if we change the interface for the
  ## sequential version too?
  rng_state <- unlist(lapply(state, "[[", "rng"))
  rng <- monty_rng$new(unlist(rng_state), n_chains)

  ## This is the inverse of restart_data really
  pars <- matrix(vapply(state, function(x) x$chain$pars, numeric(n_pars)),
                 n_pars, n_chains)
  density <- vnapply(state, function(x) x$chain$density)
  chain_state <- list(pars = pars, density = density, observation = NULL)

  sampler_state <- lapply(state, function(x) x$sampler)
  sampler$set_internal_state(sampler_state)

  stopifnot(!model$properties$is_stochastic)
  ## Need to use model$rng_state$set to put state$model_rng into the model

  monty_run_chains_simultaneous2(chain_state, model, sampler,
                                 steps, progress, rng, r_rng_state)
}


monty_run_chains_simultaneous2 <- function(chain_state, model, sampler,
                                           steps, progress, rng,
                                           r_rng_state) {
  initial <- chain_state$pars
  n_pars <- length(model$parameters)
  n_chains <- length(chain_state$density)
  n_steps_record <- steps$total

  history_pars <- array(NA_real_, c(n_pars, n_steps_record, n_chains))
  history_density <- matrix(NA_real_, n_steps_record, n_chains)

  for (i in seq_len(steps$total)) {
    chain_state <- sampler$step(chain_state, model, rng)
    history_pars[, i, ] <- chain_state$pars
    history_density[i, ] <- chain_state$density
    ## TODO: also allow observations here if enabled
    progress(i)
  }

  ## Pop the parameter names on last
  rownames(history_pars) <- model$parameters

  ## I'm not sure about the best name for this
  details <- sampler$finalise(chain_state, model, rng)

  ## This simplifies handling later; we might want to make a new
  ## version of asplit that does not leave stray attributes on later
  ## though?
  rng_state <- lapply(asplit(matrix(rng$state(), ncol = n_chains), 2),
                      as.vector)

  ## TODO: observation finalisation; this will be weird
  internal <- list(
    used_r_rng = !identical(get_r_rng_state(), r_rng_state),
    state = list(
      chain = chain_state,
      rng = rng_state,
      sampler = sampler$get_internal_state(),
      simultaneous = TRUE,
      model_rng = if (model$properties$is_stochastic) model$rng_state$get()))

  list(initial = initial,
       pars = history_pars,
       density = history_density,
       details = details,
       observations = NULL,
       internal = internal)
}
