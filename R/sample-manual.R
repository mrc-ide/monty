##' Run an MCMC, but schedule execution of the chains yourself.  Use
##' this if you want to distribute chains over (say) the nodes of an
##' HPC system.  The arguments are the same as for [monty_sample],
##' except that the `runner` argument is missing as you will be
##' looking after that yourself.  After using this function, you will
##' generally be wanting to run [monty_sample_manual_run] and
##' [monty_sample_manual_collect].
##'
##' In contrast to [monty_sample] there is no `runner` argument
##' here, because by using this function directly you are taking
##' responsibility for being your own runner.
##'
##' As with the ways of running a set of chains in monty, it is
##' expected that using `monty_sample_manual_*` will result in the
##' same samples being generated as if you had used `monty_sample`
##' with a runner of your choice.
##'
##' @title Prepare to sample with manual scheduling
##'
##' @inheritParams monty_sample
##'
##' @param path The path to write inputs and outputs to.  This should
##'   be a path to a directory which does not yet exist, or which is
##'   empty; we will create one here.  The contents of this directory
##'   are managed by `monty` and the names and contents of files here
##'   are an implementation detail and should not be relied on.
##'   Calling `monty_sample_manual_cleanup()` will delete the
##'   directory in its entirety.  Be aware that if you use
##'   [tempfile()] here (which can be a reasonable choice!) that this
##'   path will be deleted when your R process ends, so if using these
##'   the process calling `monty_sample_manual_prepare` should outlive
##'   running all sampling.
##'
##' @return Invisibly, the path used to store files (the same as the
##'   value of the `path` argument)
##'
##' @seealso [monty_sample_manual_run] to run the chains and
##'   [monty_sample_manual_collect] / [monty_sample_manual_cleanup] to
##'   collect results and clean up.  [monty_sample_manual_info] can
##'   print human-readable information about the state of a manual
##'   run.
##'
##' @export
##' @examples
##'
##' model <- monty_example("banana")
##' sampler <- monty_sampler_random_walk(vcv = diag(2) * 0.05)
##' path <- tempfile()
##' monty_sample_manual_prepare(model, sampler, 100, path)
##' monty_sample_manual_info(path)
##'
##' # Run the (single) chain
##' monty_sample_manual_run(1, path)
##' monty_sample_manual_info(path)
##'
##' # Collect the results
##' monty_sample_manual_collect(path)
##'
##' # Clean up samples
##' monty_sample_manual_cleanup(path)
monty_sample_manual_prepare <- function(model, sampler, n_steps, path,
                                        initial = NULL, n_chains = 1L,
                                        observer = NULL) {
  ## This break exists to hide the 'seed' argument from the public
  ## interface.  We will use this from the callr version though.
  sample_manual_prepare(model, sampler, n_steps, path, initial, n_chains,
                        observer)
}


sample_manual_prepare <- function(model, sampler, n_steps, path, initial,
                                  n_chains, observer, seed = NULL,
                                  call = parent.frame()) {
  assert_is(model, "monty_model", call = call)
  assert_is(sampler, "monty_sampler", call = call)
  if (!is.null(observer)) {
    assert_is(observer, "monty_observer", call = call)
  }

  rng <- initial_rng(n_chains, seed = seed)
  pars <- initial_parameters(initial, model, rng, environment())
  rng_state <- lapply(rng, function(r) r$state())

  dat <- list(pars = pars,
              model = model,
              sampler = sampler,
              observer = observer,
              rng_state = rng_state,
              n_chains = n_chains,
              n_steps = n_steps)

  sample_manual_path_create(path, dat, call = call)
}


##' Run a chain that was prepared using [monty_sample_manual_prepare].
##'
##' # Warning:
##'
##' There is no lock mechanism; you can start a single chain many
##' times.  Don't do this.
##'
##' @title Run sample with manual scheduling
##'
##' @param path The path used in the call to
##'   [monty_sample_manual_prepare]
##'
##' @param chain_id The id for the chain to run, an integer.  If you
##'   provide an integer that does not correspond to a chain in 1 to
##'   `n_chains` (where `n_chains` was the argument passed to
##'   `monty_sample_manual_prepare` it is an error.
##'
##' @inheritParams monty_runner_serial
##'
##' @export
##' @inherit monty_sample_manual_prepare examples
monty_sample_manual_run <- function(chain_id, path, progress = NULL) {
  assert_scalar_size(chain_id, allow_zero = FALSE)
  assert_scalar_character(path)
  path <- sample_manual_path(path, chain_id)

  inputs <- readRDS(path$inputs)
  if (chain_id > inputs$n_chains) {
    cli::cli_abort("'chain_id' must be an integer in 1..{inputs$n_chains}")
  }

  n_chains <- inputs$n_chains
  n_steps <- inputs$n_steps

  restart <- inputs$restart
  is_continue <- is.list(restart)

  pb <- progress_bar(n_chains, n_steps, progress,
                     show_overall = FALSE, single_chain = TRUE)(chain_id)

  if (is_continue) {
    state <- restart$state
    model <- restart$model
    sampler <- restart$sampler
    observer <- restart$observer
    res <- monty_continue_chain(state[[chain_id]], model, sampler, observer,
                                n_steps, pb)
  } else {
    pars <- inputs$pars
    model <- inputs$model
    sampler <- inputs$sampler
    observer <- inputs$observer
    rng <- monty_rng$new(seed = inputs$rng_state[[chain_id]])
    res <- monty_run_chain(pars[, chain_id], model, sampler, observer,
                           n_steps, pb, rng)
  }

  saveRDS(res, path$results)
  invisible(path$results)
}


##' Collect samples from chains that have been run with
##' [monty_sample_manual_prepare] and [monty_sample_manual_run].  If
##' any chain has not completed, we will error.
##'
##' @title Collect manually run samples
##'
##' @inheritParams monty_sample_manual_run
##'
##' @param samples Samples from the parent run.  You need to provide
##'   these where `save_samples` was set to anything other than "value"
##'
##' @param restartable Logical, indicating if the chains should be
##'   restartable.  This will add additional data to the chains
##'   object.  Note that this is controlled at chain collection and
##'   not creation.
##'
##' @return A `monty_samples` object.
##'
##' @export
##' @inherit monty_sample_manual_prepare examples
monty_sample_manual_collect <- function(path, samples = NULL,
                                        restartable = FALSE) {
  inputs <- readRDS(sample_manual_path(path)$inputs)
  path <- sample_manual_path(path, seq_len(inputs$n_chains))

  msg <- !file.exists(path$results)
  if (any(msg)) {
    cli::cli_abort("Results missing for chain{?s} {as.character(which(msg))}")
  }

  prev <- sample_manual_collect_check_samples(inputs, samples)

  res <- lapply(path$results, readRDS)
  samples <- combine_chains(res, inputs$observer)
  if (!is.null(prev)) {
    samples <- append_chains(prev, combine_chains(res, inputs$observer),
                             inputs$observer)
  }

  if (restartable) {
    samples$restart <- restart_data(res, inputs$model, inputs$sampler,
                                    inputs$observer, NULL)
  }
  samples
}


##' Clean up after manual sampling.  This is essentially a safe
##' version of deleting the directory (e.g, `unlink(path, recursive =
##' TRUE)`) which checks that the directory really was used for
##' sampling and that it does not contain anything else unexpected.
##'
##' @title Clean up samples
##'
##' @inheritParams monty_sample_manual_run
##'
##' @return Nothing, called for side effects only.
##' @export
##' @inherit monty_sample_manual_prepare examples
monty_sample_manual_cleanup <- function(path) {
  path <- sample_manual_path(path)
  inputs <- readRDS(path$inputs)
  path <- sample_manual_path(path$root, seq_len(inputs$n_chains))
  unlink(c(path$inputs, path$results))
  if (is_empty_directory(path$root)) {
    unlink(path$root, recursive = TRUE)
  } else {
    cli::cli_alert_warning(
      "Not deleting '{.path {path$root}}' as it is not empty")
  }
}


##' Get information about the status of manually scheduled samples.
##'
##' @title Get information about manually scheduled samples
##'
##' @inheritParams monty_sample_manual_run
##'
##' @return Invisibly, a logical vector, `TRUE` for completed chains
##'   and `FALSE` for incomplete chains.
##'
##' @export
##' @inherit monty_sample_manual_prepare examples
monty_sample_manual_info <- function(path) {
  path <- sample_manual_path(path)
  inputs <- readRDS(path$inputs)
  n_chains <- inputs$n_chains
  path <- sample_manual_path(path$root, seq_len(inputs$n_chains))
  done <- file.exists(path$results)
  cli::cli_h1("Manual monty sampling at {.path {path$root}}")
  cli::cli_alert_info("Created {format(file.info(path$inputs)$ctime)}")
  cli::cli_alert_info("{inputs$n_steps} steps x {n_chains} chains")
  if (is.list(inputs$restart)) {
    cli::cli_alert_info("This is a restart")
  }
  complete <- file.exists(path$results)
  sample_manual_info_chain(complete)
  invisible(complete)
}


sample_manual_info_chain <- function(complete) {
  if (all(complete)) {
    cli::cli_alert_success("All chains complete")
  } else if (!any(complete)) {
    cli::cli_alert_danger("No chains complete")
  } else {
    n_chains <- length(complete)
    cli::cli_alert_success("{sum(complete)} chain{?s} complete")
    cli::cli_alert_danger("{sum(!complete)} chain{?s} pending")
  }
}


##' Prepare to continue sampling from a model, with manual chain
##' orchestration.  This function is to [monty_sample_continue] what
##' [monty_sample_manual_prepare] is to [monty_sample].  The original
##' set of samples do not need to have been run manually.
##'
##' @title Prepare to continue sampling with manual scheduling
##'
##' @param save_samples Control over saving samples into the inputs.
##'   The choices here are `hash` (the default) where we save a hash
##'   and validate that in [monty_sample_manual_collect], `value`
##'   where the samples are themselves saved and you can omit the
##'   `samples ` argument to [monty_sample_manual_collect], or
##'   `nothing`, in which we save nothing, and you just have to get it
##'   right.
##'
##' @inheritParams monty_sample_manual_prepare
##' @inheritParams monty_sample_continue
##'
##' @export
##' @inherit monty_sample_manual_prepare return
monty_sample_manual_prepare_continue <- function(samples, n_steps, path,
                                                 save_samples = "hash") {
  ## I am not terribly happy wih the function name here, something for
  ## the review?
  ##
  ## also not happy with save_samples = "nothing"
  restart <- samples$restart
  samples <- sample_manual_prepare_check_samples(samples, save_samples)

  dat <- list(restart = restart,
              n_chains = length(restart$state),
              n_steps = n_steps,
              samples = samples)
  sample_manual_path_create(path, dat)
}


sample_manual_path_create <- function(path, dat, call = parent.frame()) {
  assert_scalar_character(path, call = call)
  exists <- file.exists(path) && !is_empty_directory(path)
  if (exists) {
    cli::cli_abort(
      c(paste("Can't use '{path}' as input for 'monty_sample_manual_prepare()'",
              "as it already exists"),
        i = paste("'path' should be an empty directly, because we will write",
                  "several files here and we want to make sure that they are",
                  "consistent")),
      call = call)
  }
  dir_create(path)
  path <- sample_manual_path(path, check = FALSE)

  ## NOTE: The 'suppressWarnings' avoids warnings about monty not
  ## being available on load.
  suppressWarnings(saveRDS(dat, path$inputs))
  invisible(path$root)
}


sample_manual_path <- function(path, chain_id = NULL, check = TRUE) {
  path_inputs <- file.path(path, "inputs.rds")
  if (check && !file.exists(path_inputs)) {
    cli::cli_abort(
      "Did not find '{basename(path_inputs)}' within '{path}'")
  }

  list(root = path,
       inputs = path_inputs,
       results = file.path(path, sprintf("results_%d.rds", chain_id)))
}


sample_manual_prepare_check_samples <- function(samples, save_samples,
                                                call = parent.frame()) {
  match_value(save_samples, c("hash", "value", "nothing"), call = call)

  restart <- samples$restart
  if (save_samples == "nothing") {
    if (inherits(samples, "monty_samples")) {
      check_can_continue_samples(samples, call = call)
    } else {
      ## This is a very relaxed check; rather than trying to get a
      ## full samples object that is restartable we quietly just look
      ## for the restart information.  We might tidy this up later...
      assert_list(restart, name = "samples$restart", arg = "samples",
                  call = call)
    }
    samples <- NULL
    samples_hash <- NULL
  } else {
    check_can_continue_samples(samples, call = call)
    if (save_samples == "hash") {
      samples_hash <- rlang::hash(samples)
      samples <- NULL
    } else { # > save_samples == "value"
      samples_hash <- NULL
    }
  }

  list(value = samples,
       hash = samples_hash)
}


sample_manual_collect_check_samples <- function(inputs, samples,
                                                call = parent.frame()) {
  if (!is.list(inputs$restart)) {
    if (!is.null(samples)) {
      cli::cli_abort(
        c("'samples' provided, but this was not a restarted sample",
          i = paste("You have provided argument 'samples' but this",
                    "manual run was started with",
                    "{.help monty_sample_manual_prepare}, rather than",
                    "{.help monty_sample_manual_prepare_continue}")),
        arg = "samples", call = call)
    }
  } else if (is.null(samples)) {
    if (!is.null(inputs$samples$value)) {
      samples <- inputs$samples$value
    } else {
      cli::cli_abort(
        "Expected 'samples' to be provided, as this chain is a continuation",
        arg = "samples", call = call)
    }
  } else {
    samples_ok <-
      identical(inputs$samples$value, samples) ||
      identical(inputs$samples$hash, rlang::hash(samples))
    if (!samples_ok) {
      cli::cli_abort(
        "Provided 'samples' does not match those at the start of the chain",
        arg = "samples", call = call)
    }
  }
  samples
}
