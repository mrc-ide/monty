##' Sample from a model.  Uses a Monte Carlo method (or possibly
##' something else in future) to generate samples from your
##' distribution.  This is going to change a lot in future, as we add
##' support for distributing over workers, and for things like
##' parallel reproducible streams of random numbers.  For now it just
##' runs a single chain as a proof of concept.
##'
##' @title Sample from a model
##'
##' @param model The model to sample from; this should be a
##'   `mcstate_model` for now, but we might change this in future to
##'   test to see if things match an interface rather than a
##'   particular class attribute.
##'
##' @param sampler A sampler to use.  These will be described later,
##'   but we hope to make these reasonably easy to implement so that
##'   we can try out different sampling ideas.  For now, the only
##'   sampler implemented is [mcstate_sampler_random_walk()].
##'
##' @param n_steps The number of steps to run the sampler for.
##'
##' @param initial Optionally, initial parameter values for the
##'   sampling.  If not given, we sample from the model (or its prior).
##'
##' @param n_chains Number of chains to run.  The default is to run a
##'   single chain, but you will likely want to run more.
##'
##' @param runner A runner for your chains.  The default option is to
##'   run chains in series (via [mcstate_runner_serial]).  The only
##'   other current option is [mcstate_runner_parallel] which uses the
##'   `parallel` package to run chains in parallel.  If you only run
##'   one chain then this argument is best left alone.
##'
##' @param restartable Logical, indicating if the chains should be
##'   restartable.  This will add additional data to the chains
##'   object.
##'
##' @return A list of parameters and densities; we'll write tools for
##'   dealing with this later.  Elements include:
##'
##' * `pars`: A matrix with as many columns as you have parameters, and
##'   as many rows as the total number of samples taken across all
##'   chains (`n_steps * n_chains`)
##'
##' * `density`: A vector of model log densities, one per step (length
##'   `n_steps * n_chains`)
##'
##' * `initial`: A record of the initial conditions, a matrix with as
##'   many columns as you have parameters and `n_chains` rows
##'
##' * `details`: Additional details reported by the sampler; this will
##'   be a list of length `n_chains` (or `NULL`) and the details
##'   depend on the sampler.  This one is subject to change.
##'
##' * `chain`: An integer vector indicating the chain that the samples
##'   came from (1, 2, ..., `n_chains`)
##'
##' @export
mcstate_sample <- function(model, sampler, n_steps, initial = NULL,
                           n_chains = 1L, runner = NULL,
                           restartable = FALSE) {
  if (!inherits(model, "mcstate_model")) {
    cli::cli_abort("Expected 'model' to be an 'mcstate_model'",
                   arg = "model")
  }
  if (!inherits(sampler, "mcstate_sampler")) {
    cli::cli_abort("Expected 'sampler' to be an 'mcstate_sampler'",
                   arg = "sampler")
  }
  if (is.null(runner)) {
    runner <- mcstate_runner_serial()
  }
  if (!inherits(runner, "mcstate_runner")) {
    cli::cli_abort("Expected 'runner' to be an 'mcstate_runner'",
                   arg = "runner")
  }

  rng <- initial_rng(n_chains)
  pars <- initial_parameters(initial, model, rng, environment())
  res <- runner$run(pars, model, sampler, n_steps, rng)

  samples <- combine_chains(res)
  if (restartable) {
    samples$restart <- restart_data(res, model, sampler, runner)
  }
  samples
}


##' Continue (restart) chains started by [mcstate_sample].  Requires
##' that the original chains were run with `restartable = TRUE`.
##' Running chains this way will result in the final state being
##' exactly the same as running for the total (original + continued)
##' number of steps in a single push.
##'
##' @title Continue sampling
##'
##' @param samples A `mcstate_samples` object created by
##'   [mcstate_sample()]
##'
##' @param n_steps The number of new steps to run
##'
##' @inheritParams mcstate_sample
##'
##' @return A list of parameters and densities
##' @export
mcstate_sample_continue <- function(samples, n_steps, restartable = FALSE) {
  if (!inherits(samples, "mcstate_samples")) {
    cli::cli_abort("Expected 'samples' to be an 'mcstate_samples' object")
  }
  if (is.null(samples$restart)) {
    cli::cli_abort(
      c("Your chains are not restartable",
        i = paste("To work with 'mcstate_sample_continue', you must",
                  "use the argument 'restartable = TRUE' when calling",
                  "mcstate_sample()")))
  }

  rng <- lapply(samples$restart$rng_state,
                function(s) mcstate_rng$new(seed = s))
  model <- samples$restart$model
  pars <- samples$restart$pars
  sampler <- samples$restart$sampler
  runner <- samples$restart$runner

  res <- runner$run(pars, model, sampler, n_steps, rng)
  samples <- append_chains(samples, combine_chains(res))

  if (restartable) {
    samples$restart <- restart_data(res, model, sampler, runner)
  }
  samples
}


mcstate_sampler <- function(name, initialise, step, finalise) {
  ret <- list(name = name,
              initialise = initialise,
              step = step,
              finalise = finalise)
  class(ret) <- "mcstate_sampler"
  ret
}


initial_parameters <- function(initial, model, rng, call = NULL) {
  n_pars <- length(model$parameters)
  n_chains <- length(rng)
  if (is.null(initial)) {
    require_direct_sample(model,
                          "'initial' must be provided with this model",
                          arg = "initial", call = environment())
    ## Really this would just be from the prior; we can't directly
    ## sample from the posterior!
    initial <- lapply(rng, function(r) model$direct_sample(r))
  }
  if (is.list(initial)) {
    if (length(initial) != n_chains) {
      cli::cli_abort(
        c(paste("Unexpected length for list 'initial'",
                "(given {length(initial)}, expected {n_chains})"),
          i = paste("You are running {n_chains}, so I expected a list with",
                    "that many entries, each of which has {n_pars}",
                    "element{?s}")),
        arg = "initial", call = call)
    }
    err <- lengths(initial) != n_pars
    if (any(err)) {
      ## The format here is not beautiful but it's fairly informative
      ## at least.
      len <- as.character(lengths(initial)[err])
      len[[1]] <- paste(len[[1]], ngettext(len[[1]], "value", "values"))
      detail <- sprintf("%d (%s)", which(err), len)
      cli::cli_abort(
        c(paste("Unexpected initial parameter length; expected {n_pars}"),
          i = "Incorrect length for element{?s} {detail}",
          i = paste("Your model has {n_pars} parameter{?s}, so each",
                    "element of your list must have this many elements")),
        arg = "initial", call = call)
    }
    initial <- matrix(unlist(unname(initial)), n_chains, n_pars, byrow = TRUE)
  } else if (is.matrix(initial)) {
    hint_matrix <- paste(
      "Expected a matrix with {n_chains} row{?s} (1 per chain) and",
      "{n_pars} column{?s} (1 per parameter)")
    if (nrow(initial) != n_chains) {
      cli::cli_abort(
        c(paste("Unexpected number of rows in 'initial'",
                "(given {nrow(initial)}, expected {n_chains})"),
          i = hint_matrix),
        arg = "initial", call = call)
    }
    if (ncol(initial) != n_pars) {
      cli::cli_abort(
        c(paste("Unexpected number of columns in 'initial'",
                "(given {ncol(initial)}, expected {n_pars})"),
          i = hint_matrix),
        arg = "initial", call = call)
    }
  } else {
    if (length(initial) != n_pars) {
      cli::cli_abort(
        c(paste("Unexpected length for vector 'initial'",
                "(given {length(initial)}, expected {n_pars})"),
          i = "I expected a vector with {n_pars} element{?s}, 1 per parameter"),
        arg = "initial", call = call)
    }
    initial <- matrix(initial, n_chains, n_pars, byrow = TRUE)
  }
  initial
}


combine_chains <- function(res) {
  stopifnot(is.null(res$chain))
  chain <- rep(seq_along(res), lengths(lapply(res, "[[", "density")))

  pars <- rbind_list(lapply(res, "[[", "pars"))
  density <- unlist(lapply(res, "[[", "density"))
  details <- lapply(res, "[[", "details")
  details <- if (all(vlapply(details, is.null))) NULL else details

  n_pars <- ncol(pars)
  initial <- vapply(res, "[[", numeric(n_pars), "initial")
  if (n_pars == 1) {
    initial <- cbind(initial)
  } else {
    initial <- t(initial)
  }
  colnames(initial) <- colnames(pars)

  used_r_rng <- vlapply(res, function(x) x$internal$used_r_rng)
  if (any(used_r_rng)) {
    cli::cli_warn(c(
      "Detected use of R's random number generators",
      i = paste("Your model has used R's random number generators (e.g.,",
                "via rnorm, runif, sample, etc).  This means that your",
                "results will not be reproducible as you change the sample",
                "runner")))
  }

  samples <- list(pars = pars,
                  density = density,
                  initial = initial,
                  details = details,
                  chain = chain)
  class(samples) <- "mcstate_samples"
  samples
}


## This is absolutely terrible, but it will get there.
append_chains <- function(prev, curr) {
  n_chains <- length(prev$restart$rng_state)
  i <- split(seq_along(prev$chain), prev$chain)
  j <- split(seq_along(curr$chain) + length(prev$chain), curr$chain)
  k <- unlist(rbind(i, j))

  pars <- rbind(prev$pars, curr$pars)[k, , drop = FALSE]
  density <- c(prev$density, curr$density)[k]
  if (!is.null(prev$details) || !is.null(curr$details)) {
    ## This needs to wait until hmc or adaptive sampling are merged to
    ## work with.
    cli::cli_abort("Can't yet merge chains with details")
  } else {
    details <- NULL
  }
  chain <- c(prev$chain, curr$chain)[k]

  samples <- list(pars = pars,
                  density = density,
                  initial = prev$initial,
                  details = details,
                  chain = chain)
  class(samples) <- "mcstate_samples"
  samples
}


initial_rng <- function(n_chains, seed = NULL) {
  lapply(mcstate_rng_distributed_state(n_nodes = n_chains, seed = seed),
         function(s) mcstate_rng$new(seed = s))
}


restart_data <- function(res, model, sampler, runner) {
  ## TODO: thisis not actually enough; we also need the state from any
  ## stateful sampler (so that's the case for the adaptive sampler and
  ## for hmc with debug enabled)
  n_pars <- ncol(res[[1]]$pars)
  pars <- vapply(res, function(x) x$pars[nrow(x$pars), ], numeric(n_pars))
  if (n_pars == 1) {
    pars <- matrix(pars, ncol = 1)
  }
  list(rng_state = lapply(res, function(x) x$internal$rng_state),
       pars = pars,
       model = model,
       sampler = sampler,
       runner = runner)
}
