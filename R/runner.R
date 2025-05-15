##' Run MCMC chains in series (one after another).  This is the
##' simplest chain runner, and the default used by [monty_sample()].
##' It has nothing that can be configured (yet).
##'
##' @title Run MCMC chain in series
##'
##' @param progress Optional logical, indicating if we should print a
##'   progress bar while running.  If `NULL`, we use the value of the
##'   option `monty.progress` if set, otherwise we show the progress
##'   bar (as it is typically wanted).  Alternatively, you can provide
##'   a string indicating the progress bar type.  Options are `fancy`
##'   (equivalent to `TRUE`), `none` (equivalent to `FALSE`) and
##'   `simple` (a very simple text-mode progress indicator designed
##'   play nicely with logging; it does not use special codes to clear
##'   the line).
##'
##' @return A runner of class `monty_runner` that can be passed to
##'   [monty_sample()]
##'
##' @export
monty_runner_serial <- function(progress = NULL) {
  run <- function(pars, model, sampler, steps, rng) {
    n_chains <- length(rng)
    pb <- progress_bar(n_chains, steps$total, progress, show_overall = TRUE)
    with_progress_fail_on_error(
      pb,
      lapply(
        seq_along(rng),
        function(i) {
          monty_run_chain(i, pars[, i], model, sampler, steps,
                          pb$update, rng[[i]])
        }))
  }

  continue <- function(state, model, sampler, steps) {
    n_chains <- length(state)
    pb <- progress_bar(n_chains, steps$total, progress, show_overall = TRUE)
    with_progress_fail_on_error(
      pb,
      lapply(
        seq_along(state),
        function(i) {
          monty_continue_chain(i, state[[i]], model, sampler, steps, pb$update)
        }))
  }

  monty_runner("Serial",
               "monty_runner_serial",
               run,
               continue)
}


##' Run MCMC chains in parallel (at the same time).  This runner uses
##' the `parallel` package to distribute your chains over a number of
##' worker processes on the same machine.  Compared with
##' [monty_runner_callr] (Which is similar to the "worker" support in
##' `mcstate` version 1), this is very simple.  In particular we do
##' not report back any information about progress while a chain is
##' running on a worker or even across chains.  There's also no
##' support to warn you if your number of chains do not neatly divide
##' through by the number of workers.  Mostly this exists as a proof
##' of concept for us to think about the different interfaces.  Unless
##' your chains are quite slow, the parallel runner will be slower
##' than the serial runner ([monty_runner_serial]) due to the overhead
##' cost of starting the cluster.
##'
##' @title Run MCMC chain in parallel
##'
##' @param n_workers Number of workers to create a cluster from.  In a
##'   multi-user setting be careful not to set this to more cores than
##'   you are allowed to use.  You can use `parallel::detectCores()`
##'   to get an estimate of the number of cores you have on a single
##'   user system (but this is often an overestimate as it returns the
##'   number of logical cores, including those from "hyperthreading").
##'   Fewer cores than this will be used if you run fewer chains than
##'   you have workers.
##'
##' @return A runner of class `monty_runner` that can be passed to
##'   [monty_sample()]
##'
##' @export
monty_runner_parallel <- function(n_workers) {
  ## TODO: accept a cluster as an optional argument here, I think.
  ## There is an interface we can use easily.

  ## An alternative way of doing this would be to start the cluster
  ## immediately on creation of the runner object, which means that
  ## subsequent uses would reuse a cluster; that would probably be
  ## nicer, but slightly harder.  If we took that approach for a callr
  ## based backend later (similar to what mcstate1 does) then we'll
  ## get the advantage that the cluster startup happens asyncronously
  ## and may be ready by the time we actually pass any work onto it.

  run <- function(pars, model, sampler, steps, rng) {
    n_chains <- length(rng)
    cl <- parallel::makeCluster(min(n_chains, n_workers))
    on.exit(parallel::stopCluster(cl))
    ## There is a good chance the user will see warnings about
    ## "package X not being available", and also a chance that objects
    ## will not survive serialisation/deserialisation between
    ## processes here - but we'll face that on basically every
    ## possible parallel backend really.
    ##
    ## It would be nice to refactor the rng so that we are using
    ## something closer to the rng pointer object rather than the big
    ## heavy R6 object as that already has the logic in for doing
    ## state sync, but this can all be done transparently later.
    pars_list <- asplit(pars, MARGIN = 2)
    rng_state <- lapply(rng, function(r) monty_rng_state(r))

    args <- list(model = model,
                 sampler = sampler,
                 steps = steps)

    ## To debug issues in the parallel sampler, it's most efficient to
    ## replace this call with `Map` and drop the `cl` argument, then
    ## you get nice stack traces.
    parallel::clusterMap(
      cl,
      monty_run_chain_parallel,
      chain_id = seq_len(n_chains),
      pars = pars_list,
      rng = rng_state,
      MoreArgs = args)
  }

  continue <- function(state, model, sampler, steps) {
    n_chains <- length(state)
    cl <- parallel::makeCluster(min(n_chains, n_workers))
    on.exit(parallel::stopCluster(cl))
    args <- list(model = model,
                 sampler = sampler,
                 steps = steps,
                 progress = progress_bar_none()$update)
    parallel::clusterMap(
      cl,
      monty_continue_chain,
      chain_id = seq_len(n_chains),
      state,
      MoreArgs = args)
  }

  monty_runner("Parallel",
               "monty_runner_parallel",
               run,
               continue)
}


monty_run_chain_parallel <- function(chain_id, pars, model, sampler, steps,
                                     rng) {
  rng <- monty_rng_create(seed = rng)
  progress <- progress_bar_none()$update
  monty_run_chain(chain_id, pars, model, sampler, steps, progress, rng)
}


monty_run_chain <- function(chain_id, pars, model, sampler, steps,
                            progress, rng) {
  r_rng_state <- get_r_rng_state()

  ## NOTE: sampler$inputs is really a non-mutable set of inputs to the
  ## sampler - I'm not totally sure why we are accessing it this way,
  ## but I think it's because the actual sampler objects are stateless
  ## and this gives access to the arguments later on.
  shared <- monty_sampler_shared(model, sampler$inputs, rng)

  ## TODO: I am not convinced that 'internal' is actually anything
  ## real here, and that we might merge 'shared+internal' as 'state'?
  ## That's a fairly easy fix to do once we get through this first bit
  ## of refactoring though.
  internal <- new.env(parent = emptyenv())

  n_chains <- length(chain_id)
  sampler$begin(shared, internal, pars, n_chains)

  if (!is.finite(shared$density)) {
    ## Ideally, we'd do slightly better than this; it might be worth
    ## validing this even earlier (in the parameter validation step)
    ## but that has its own issues (e.g., in the heirarchical sampler
    ## we can't extract out the submodel densities, and we might have
    ## issues with rng behaviour of stochastic models).  So this way
    ## around we'll fail the chain, which is sad if it's a chain that
    ## only starts running after several hours, and particularly in
    ## parallel we won't find out that anything (and therefore
    ## everything) has failed until everything has completed.
    ##
    ## Once we formalise the idea of restartable model runners though,
    ## this will come out in the wash I think as we can initialise all
    ## our chains, then "restart" them from the first point; that is
    ## hard to do with the random walk sampler as it's stateless so
    ## there's no real effect, but we'll readdress this once we get
    ## the HMC or adaptive samplers set up.
    cli::cli_abort("Chain does not have finite starting density")
  }

  ## This passing of state (the sampler is now three pieces -
  ## {Sampler, Shared, State}) is somewhat peculiar and we should join
  ## these in a little list that formalises the above.
  monty_run_chain2(chain_id, sampler, shared, internal, steps, progress,
                   r_rng_state)
}


monty_continue_chain <- function(chain_id, state, model, sampler, steps,
                                 progress) {
  r_rng_state <- get_r_rng_state()
  rng <- monty_rng_create(seed = state$rng)

  internal <- list2env(state$sampler, parent = emptyenv())
  shared <- monty_sampler_shared(model, sampler$inputs, rng)
  ## This really wants us to write the PT one next, because at this
  ## point we need to restore the chain state.
  ##
  ## For the RW sampler this is pars/density only but for PT it's a
  ## few other bits, too.
  list2env(state$chain, shared)
  sampler$resume(state, internal)

  ## Here, it looks like we have shared$pars and shared$density and
  ## not shared$state$pars/shared$state$density, which in turn breaks
  ## the proposal.  I think we should drop 'state' from here as it's
  ## not helpful and then try and refactor.

  if (model$properties$is_stochastic) {
    shared$model$rng_state$set(state$model_rng)
  }
  monty_run_chain2(chain_id, sampler, shared, internal, steps, progress,
                   r_rng_state)
}


monty_run_chain2 <- function(chain_id, sampler, shared, internal, steps,
                             progress, r_rng_state) {
  initial <- shared$pars
  model <- shared$model
  n_pars <- length(model$parameters)
  has_observer <- model$properties$has_observer

  burnin <- steps$burnin
  thinning_factor <- steps$thinning_factor
  n_steps <- steps$total
  n_steps_record <- ceiling((steps$total - burnin) / thinning_factor)

  history_pars <- matrix(NA_real_, n_pars, n_steps_record)
  history_density <- rep(NA_real_, n_steps_record)
  history_observation <-
    if (has_observer) vector("list", n_steps_record) else NULL

  j <- 1L
  for (i in seq_len(n_steps)) {
    sampler$step(shared, internal)
    if (i > burnin && i %% thinning_factor == 0) {
      history_pars[, j] <- shared$pars
      history_density[[j]] <- shared$density
      if (has_observer && !is.null(shared$observation)) {
        history_observation[[j]] <- shared$observation
      }
      j <- j + 1L
    }
    progress(chain_id, i)
  }

  ## Pop the parameter names on last
  rownames(history_pars) <- model$parameters

  details <- sampler$details(shared, internal)

  if (has_observer) {
    history_observation <- model$observer$finalise(history_observation)
  }

  ## This list will hold things that we'll use internally but not
  ## surface to the user in the final object (or summarise them in
  ## some particular way with no guarantees about the format).  We
  ## might hold things like start and stop times here in future.
  internal <- list(
    used_r_rng = !identical(get_r_rng_state(), r_rng_state),
    state = list(
      chain = list(pars = shared$pars, density = shared$density),
      rng = monty_rng_state(shared$rng),
      sampler = sampler$internal_state(shared, internal),
      model_rng = if (model$properties$is_stochastic) model$rng_state$get()))

  list(initial = initial,
       pars = history_pars,
       density = history_density,
       details = details,
       observations = history_observation,
       internal = internal)
}


monty_runner <- function(name, help, run, continue) {
  ret <- list(name = name,
              help = help,
              run = run,
              continue = continue)
  class(ret) <- "monty_runner"
  ret
}


##' @export
print.monty_runner <- function(x, ...) {
  cli::cli_h1("<monty_runner: {x$name} ({x$help})>")
  cli::cli_alert_info("Use {.help monty_sample} to use this runner")
  cli::cli_alert_info("See {.help {x$help}} for more information")
  invisible(x)
}
