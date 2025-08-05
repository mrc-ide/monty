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
    rng_state <- lapply(rng, function(r) monty_rng_state(r))
    ## TODO: get the rng state back into 'rng' here, or (better) look
    ## at if we should just be using seed instead here perhaps?
    ## > rng_state <- matrix(res$internal$state$rng, ncol = n_chains)
    ## > for (i in seq_len(n_chains)) {
    ## >   rng[[i]]$set_state(rng_state[, i]) # not supported!
    ## > }
    with_progress_fail_on_error(
      pb,
      monty_run_chains_simultaneous(pars, model, sampler, steps, pb$update,
                                    rng_state))
  }

  continue <- function(state, model, sampler, steps) {
    validate_suitable(model)
    n_chains <- length(state)
    pb <- progress_bar(n_chains, steps$total, progress, show_overall = FALSE)
    with_progress_fail_on_error(
      pb,
      monty_continue_chains_simultaneous(state, model, sampler, steps,
                                         pb$update))
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
  rng <- monty_rng_create(seed = unlist(rng_state), n_streams = n_chains)

  if (is_v2_sampler(sampler)) {
    chain_state <- initialise_state(pars, model, rng)
    sampler_state <-
      sampler$initialise(chain_state, sampler$control, model, rng)
  } else {
    chain_state <- sampler$initialise(pars, model, rng)
    sampler_state <- NULL
  }

  monty_run_chains_simultaneous2(chain_state, sampler_state, model, sampler,
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
  rng <- monty_rng_create(seed = unlist(rng_state), n_streams = n_chains)

  ## This is the inverse of restart_data really
  pars <- matrix(vapply(state, function(x) x$chain$pars, numeric(n_pars)),
                 n_pars, n_chains)
  density <- vnapply(state, function(x) x$chain$density)
  chain_state <- list(pars = pars, density = density, observation = NULL)

  ## We have to (at least for now) just take the first sampler state.
  ## This is not totally ideal, but most of the time the runner will
  ## be the same in which case this is the same data replicated n
  ## times.  We could warn, but as there's not a lot of better
  ## alternatives for the user, let's just keep going.
  sampler_state <- state[[1]]$sampler
  if (is_v2_sampler(sampler)) {
    sampler_state <- sampler$state$restore(
      chain_state, state$sampler, sampler$control, model)
  } else {
    if (!is.null(sampler_state)) {
      sampler$set_internal_state(sampler_state)
    }
    sampler_state <- NULL
  }

  stopifnot(!model$properties$is_stochastic)
  ## Need to use model$rng_state$set to put state$model_rng into the model

  monty_run_chains_simultaneous2(chain_state, sampler_state, model, sampler,
                                 steps, progress, rng, r_rng_state)
}


monty_run_chains_simultaneous2 <- function(chain_state, sampler_state,
                                           model, sampler,
                                           steps, progress, rng,
                                           r_rng_state) {
  initial <- chain_state$pars
  n_pars <- length(model$parameters)
  n_chains <- length(chain_state$density)
  n_steps_record <- steps$total

  history_pars <- array(NA_real_, c(n_pars, n_steps_record, n_chains))
  history_density <- matrix(NA_real_, n_steps_record, n_chains)

  chain_id <- seq_len(n_chains)
  is_v2_sampler <- is_v2_sampler(sampler)

  for (i in seq_len(steps$total)) {
    if (is_v2_sampler) {
      chain_state <- sampler$step(chain_state, sampler_state, sampler$control,
                                  model, rng)
    } else {
      chain_state <- sampler$step(chain_state, model, rng)
    }
    history_pars[, i, ] <- chain_state$pars
    history_density[i, ] <- chain_state$density
    ## TODO: also allow observations here if enabled
    progress(chain_id, i)
  }

  ## Pop the parameter names on last
  rownames(history_pars) <- model$parameters

  ## I'm not sure about the best name for this
  if (is_v2_sampler) {
    if (is.null(sampler$details)) {
      details <- NULL
    } else {
      details <- sampler$details(chain_state, sampler_state, sampler$control,
                                 model)
    }
    sampler_state <- sampler$state$dump(sampler_state)
  } else {
    details <- sampler$finalise(chain_state, model, rng)
    sampler_state <- sampler$get_internal_state()
  }

  ## This simplifies handling later; we might want to make a new
  ## version of asplit that does not leave stray attributes on later
  ## though?
  rng_state <- matrix(monty_rng_state(rng), ncol = n_chains)
  rng_state <- lapply(asplit(rng_state, 2), as.vector)

  if (!is.null(sampler_state)) {
    sampler_state <- rep(list(sampler_state), n_chains)
  }

  ## TODO: observation finalisation; this will be weird
  internal <- list(
    used_r_rng = !identical(get_r_rng_state(), r_rng_state),
    state = list(
      chain = chain_state,
      rng = rng_state,
      sampler = sampler_state,
      simultaneous = TRUE,
      model_rng = if (model$properties$is_stochastic) model$rng_state$get()))

  list(initial = initial,
       pars = history_pars,
       density = history_density,
       details = details,
       observations = NULL,
       internal = internal)
}
