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
##'   `monty_model` for now, but we might change this in future to
##'   test to see if things match an interface rather than a
##'   particular class attribute.
##'
##' @param sampler A sampler to use.  These will be described later,
##'   but we hope to make these reasonably easy to implement so that
##'   we can try out different sampling ideas.  For now, the only
##'   sampler implemented is [monty_sampler_random_walk()].
##'
##' @param n_steps The number of steps to run the sampler for.
##'
##' @param initial Optionally, initial parameter values for the
##'   sampling.  If not given, we sample from the model (or its
##'   prior).  Alternatively, you can provide a `monty_samples`
##'   object here -- the result of a previous call to this function --
##'   and we will sample some starting points from the final portion
##'   of the chains (the exact details here are subject to change, but
##'   we'll sample from the last 20 points or 5% of the chain, which
##'   ever smaller, with replacement, pooled across all chains in the
##'   previous sample).
##'
##' @param n_chains Number of chains to run.  The default is to run a
##'   single chain, but you will likely want to run more.
##'
##' @param runner A runner for your chains.  The default option is to
##'   run chains in series (via [monty_runner_serial]).  The only
##'   other current option is [monty_runner_parallel] which uses the
##'   `parallel` package to run chains in parallel.  If you only run
##'   one chain then this argument is best left alone.
##'
##' @param restartable Logical, indicating if the chains should be
##'   restartable.  This will add additional data to the chains
##'   object.
##'
##' @param burnin Number of steps to discard as burnin.  This affects
##'   only the recording of steps as your chains run; we don't record
##'   the first `burnin` steps.  Generally you would want to do this
##'   in post-processing with [monty_samples_thin()] as this data is
##'   discarded with no chance of getting it back.  However, if your
##'   observation process creates a large amount of data, then you may
##'   prefer to apply a burnin here to reduce how much memory is used.
##'
##' @param thinning_factor A thinning factor to apply while the chain
##'   is running.  If given, then we save every `thinning_factor`'th
##'   step.  So if `thinning_factor = 2` we save every second step,
##'   and if 10, we'd save every 10th.  Like `burnin` above, it is
##'   preferable to apply this in post processing with
##'   [monty_samples_thin()].  However, for slow-mixing chains that
##'   have a large observer output you can use this to reduce the
##'   memory usage.  Use of `thinning_factor` requires that `n_steps`
##'   is an even multiple of `thinning_factor`; so if
##'   `thinning_factor` is 10, then `n_steps` must be a multiple of
##'   10.  This ensures that the last step is in the sample.  The
##'   thinning factor cannot be changed when continuing a chain.
##'
##' @return A list of parameters and densities.  We provide conversion
##'   to formats used by other packages, notably
##'   [posterior::as_draws_array], [posterior::as_draws_df] and
##'   [coda::as.mcmc.list]; please let us know if you need conversion
##'   to something else.  If you want to work directly with the
##'   output, the elements in the list include:
##'
##' * `pars`: An array with three dimensions representing (in turn)
##'   parameter, sample and chain, so that `pars[i, j, k]` is the
##'   `i`th parameter from the `j`th sample from the `k`th chain.  The
##'   rows will be named with the names of the parameters, from your
##'   model.
##'
##' * `density`: A matrix of model log densities, with `n_steps` rows
##'   and `n_chains` columns.
##'
##' * `initial`: A record of the initial conditions, a matrix with as
##'   many rows as you have parameters and `n_chains` columns (this is
##'   the same format as the matrix form of the `initial` input
##'   parameter)
##'
##' * `details`: Additional details reported by the sampler; this will
##'   be a list of length `n_chains` (or `NULL`) and the details
##'   depend on the sampler.  This one is subject to change.
##'
##' * `observations`: Additional details reported by the model.  This
##'   one is also subject to change.
##'
##' @export
##' @examples
##' m <- monty_example("banana")
##' s <- monty_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)
##' samples <- monty_sample(m, s, 2000)
##'
##' # Quick conversion of parameters into something plottable:
##' pars <- t(drop(samples$pars))
##' plot(pars, pch = 19, cex = 0.75, col = "#0000ff55")
##'
##' # If you have the posterior package you might prefer converting to
##' # its format for performing diagnoses:
##' @examplesIf requireNamespace("posterior")
##' res <- posterior::as_draws_df(samples)
##' posterior::summarise_draws(res)
##'
##' # At this point you could also use the 'bayesplot' package to plot
##' # diagnostics.
monty_sample <- function(model, sampler, n_steps, initial = NULL,
                         n_chains = 1L, runner = NULL,
                         restartable = FALSE, burnin = NULL,
                         thinning_factor = NULL) {
  assert_is(model, "monty_model")
  assert_is(sampler, "monty_sampler")
  if (is.null(runner)) {
    runner <- monty_runner_serial()
  } else {
    assert_is(runner, "monty_runner")
  }
  assert_scalar_logical(restartable)

  rng <- initial_rng(n_chains)
  pars <- initial_parameters(initial, model, rng, environment())
  steps <- monty_sample_steps(n_steps, burnin, thinning_factor)
  res <- runner$run(pars, model, sampler, steps, rng)

  observer <- if (model$properties$has_observer) model$observer else NULL
  samples <- combine_chains(res, model$observer)
  if (restartable) {
    samples$restart <- restart_data(res, model, sampler, runner,
                                    thinning_factor)
  }
  samples
}


##' Continue (restart) chains started by [monty_sample].  Requires
##' that the original chains were run with `restartable = TRUE`.
##' Running chains this way will result in the final state being
##' exactly the same as running for the total (original + continued)
##' number of steps in a single push.
##'
##' @title Continue sampling
##'
##' @param samples A `monty_samples` object created by
##'   [monty_sample()]
##'
##' @param n_steps The number of new steps to run
##'
##' @param runner Optionally, a runner for your chains.  The default
##'   is to continue with the backend that you used to start the
##'   chains via [monty_sample] (or on the previous restart with
##'   this function).  You can use this argument to change the runner,
##'   which might be useful if transferring a pilot run from a
##'   high-resource environment to a lower-resource environment.  If
##'   given, must be a `monty_runner` object such as
##'   [monty_runner_serial] or [monty_runner_parallel].  You can
##'   use this argument to change the configuration of a runner, as
##'   well as the type of runner (e.g., changing the number of
##'   allocated cores).
##'
##' @param append Logical, indicating if we should append the results
##'   of the resumed chain together with the original chain.
##'
##' @inheritParams monty_sample
##'
##' @return A list of parameters and densities
##' @export
monty_sample_continue <- function(samples, n_steps, restartable = FALSE,
                                  runner = NULL, append = TRUE) {
  check_can_continue_samples(samples)
  assert_scalar_logical(restartable)
  assert_scalar_logical(append)

  if (is.null(runner)) {
    runner <- samples$restart$runner
  } else {
    assert_is(runner, "monty_runner")
  }
  state <- samples$restart$state
  model <- samples$restart$model
  sampler <- samples$restart$sampler

  burnin <- NULL
  thinning_factor <- samples$restart$thinning_factor
  steps <- monty_sample_steps(n_steps, burnin, thinning_factor)

  res <- runner$continue(state, model, sampler, steps)
  observer <- if (model$properties$has_observer) model$observer else NULL
  samples_new <- combine_chains(res, observer)

  if (append) {
    samples <- append_chains(samples, samples_new, observer)
  } else {
    samples <- samples_new
  }

  if (restartable) {
    samples$restart <- restart_data(res, model, sampler, runner,
                                    thinning_factor)
  }
  samples
}


check_can_continue_samples <- function(samples, call = parent.frame()) {
  if (!inherits(samples, "monty_samples")) {
    cli::cli_abort("Expected 'samples' to be a 'monty_samples' object",
                   call = call)
  }
  if (is.null(samples$restart)) {
    cli::cli_abort(
      c("Your chains are not restartable",
        i = paste("To work with 'monty_sample_continue', you must",
                  "use the argument 'restartable = TRUE' when calling",
                  "monty_sample()")),
      call = call)
  }
}


monty_sampler <- function(name, help, initialise, step, finalise,
                          get_internal_state, set_internal_state) {
  ret <- list(name = name,
              help = help,
              initialise = initialise,
              step = step,
              finalise = finalise,
              get_internal_state = get_internal_state,
              set_internal_state = set_internal_state)
  class(ret) <- "monty_sampler"
  ret
}


##' @export
print.monty_sampler <- function(x, ...) {
  cli::cli_h1("<monty_sampler: {x$name} ({x$help})>")
  cli::cli_alert_info("Use {.help monty_sample} to use this sampler")
  cli::cli_alert_info("See {.help {x$help}} for more information")
  invisible(x)
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
    initial <- lapply(rng, function(r) direct_sample_within_domain(model, r))
  }
  if (inherits(initial, "monty_samples")) {
    ## Heuristic here; sample from the last 5% of the chain or 20
    ## points, whichever is smaller - hopefully a reasonable
    ## heuristic - pooled across chains.
    pars <- tail_and_pool(initial$pars, 0.05, 20)
    if (nrow(pars) != n_pars) {
      cli::cli_abort(
        c(paste("Unexpected parameter length in 'monty_samples' object",
                "'initial'; expected {n_pars}"),
          i = paste("Your model has {n_pars} parameter{?s}, so the 'initial'",
                    "object must have this many rows within its 'pars'",
                    "element, but yours had {nrow(pars)} row{?s}")),
        arg = "initial", call = call)
    }
    i <- ceiling(vnapply(rng, function(r) monty_random_real(r)) * ncol(pars))
    initial <- pars[, i, drop = FALSE]
  } else if (is.list(initial)) {
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
    initial <- matrix(unlist(unname(initial)), n_pars, n_chains)
  } else if (is.matrix(initial)) {
    hint_matrix <- paste(
      "Expected a matrix with {n_pars} row{?s} (1 per parameter) and",
      "{n_chains} column{?s} (1 per chain)")
    if (nrow(initial) != n_pars) {
      cli::cli_abort(
        c(paste("Unexpected number of rows in 'initial'",
                "(given {nrow(initial)}, expected {n_pars})"),
          i = hint_matrix),
        arg = "initial", call = call)
    }
    if (ncol(initial) != n_chains) {
      cli::cli_abort(
        c(paste("Unexpected number of columns in 'initial'",
                "(given {ncol(initial)}, expected {n_chains})"),
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
    initial <- matrix(initial, n_pars, n_chains)
  }

  err <- initial < model$domain[, 1] | initial > model$domain[, 2]
  if (any(err)) {
    ## Reporting on this is hard because we want to know what chain
    ## starting point is the issue, and which parameters.
    err_parameters <- model$parameters[rowSums(err) > 0]
    hint_parameters <- "Issues with parameter{?s}: {squote(err_parameters)}"
    err_chain <- colSums(err) > 0
    if (all(err_chain) && n_chains > 1) {
      hint_chain <- "Issues with every chain"
    } else {
      hint_chain <-
        "Issues with {cli::qty(sum(err_chain))}chain{?s} {which(err_chain)}"
    }
    cli::cli_abort(
      c("Initial conditions do not fall within parameter domain",
        x = hint_parameters,
        x = hint_chain),
      arg = "initial", call = call)
  }
  initial
}


combine_chains <- function(res, observer = NULL) {
  if (is.null(names(res))) {
    pars <- array_bind(arrays = lapply(res, "[[", "pars"), after = 2)
    density <- array_bind(arrays = lapply(res, "[[", "density"), after = 1)
    details <- lapply(res, "[[", "details")
    details <- if (all(vlapply(details, is.null))) NULL else details

    n_pars <- nrow(pars)
    initial <- vapply(res, "[[", numeric(n_pars), "initial")
    if (n_pars == 1) {
      initial <- rbind(initial, deparse.level = 0)
    }

    if (is.null(observer)) {
      observations <- NULL
    } else {
      observations <- observer$combine(lapply(res, "[[", "observations"))
    }
    used_r_rng <- vlapply(res, function(x) x$internal$used_r_rng)
  } else {
    stopifnot(is.null(observer)) # prevented earlier
    pars <- res$pars
    density <- res$density
    details <- res$details
    observations <- res$observations
    used_r_rng <- res$internal$used_r_rng
    initial <- res$initial
  }

  rownames(initial) <- rownames(pars)

  if (any(used_r_rng)) {
    cli::cli_warn(c(
      "Detected use of R's random number generators",
      i = paste("Your model has used R's random number generators (e.g.,",
                "via rnorm, runif, sample, etc).  This means that your",
                "results will not be reproducible as you change the sample",
                "runner"),
      i = paste("If you are not using a parallel runner, you can debug",
                "this with 'with_trace_random()'")))
  }

  samples <- list(pars = pars,
                  density = density,
                  initial = initial,
                  details = details,
                  observations = observations)
  class(samples) <- "monty_samples"
  samples
}


append_chains <- function(prev, curr, observer = NULL) {
  if (is.null(observer)) {
    observations <- NULL
  } else {
    observations <- observer$append(prev$observations, curr$observations)
  }
  samples <- list(pars = array_bind(prev$pars, curr$pars, on = 2),
                  density = array_bind(prev$density, curr$density, on = 1),
                  initial = prev$initial,
                  details = curr$details,
                  observations = observations)
  class(samples) <- "monty_samples"
  samples
}


initial_rng <- function(n_chains, seed = NULL) {
  lapply(monty_rng_distributed_state(n_nodes = n_chains, seed = seed),
         function(s) monty_rng_create(seed = s))
}


restart_data <- function(res, model, sampler, runner, thinning_factor) {
  if (is.null(names(res))) {
    state <- lapply(res, function(x) x$internal$state)
  } else {
    ## Prevented elsewhere, requires the filter to be rewritten a bit.
    stopifnot(is.null(res$internal$state$model_rng))
    n_chains <- length(res$internal$state$chain$density)
    state <- lapply(seq_len(n_chains), function(i) {
      list(chain = list(
             pars = res$internal$state$chain$pars[, i],
             density = res$internal$state$chain$density[i],
             observation = NULL),
           rng = res$internal$state$rng[[i]],
           sampler = res$internal$state$sampler[[i]],
           model_rng = NULL)
    })
  }
  list(state = state,
       model = model,
       sampler = sampler,
       runner = runner,
       thinning_factor = thinning_factor)
}


direct_sample_within_domain <- function(model, rng, max_attempts = 100) {
  for (i in seq_len(max_attempts)) {
    x <- model$direct_sample(rng)
    if (all(x >= model$domain[, 1] & x <= model$domain[, 2])) {
      return(x)
    }
  }
  cli::cli_abort(
    c("Failed to sample initial conditions within {max_attempts} attempt{?s}",
      i = paste("Your model's 'direct_sample()' method is generating",
                "samples that fall outside your model's domain.  Probably",
                "you should fix one or both of these!")))
}


monty_sample_steps <- function(n_steps, burnin = NULL, thinning_factor = NULL,
                               call = parent.frame()) {
  if (inherits(n_steps, "monty_sample_steps")) {
    return(n_steps)
  }
  assert_scalar_size(n_steps, call = call)
  if (is.null(burnin)) {
    burnin <- 0
  } else {
    assert_scalar_size(burnin, allow_zero = TRUE, call = call)
    if (burnin >= n_steps) {
      cli::cli_abort("'burnin' must be smaller than 'n_steps'",
                     arg = "burnin", call = call)
    }
  }
  if (is.null(thinning_factor)) {
    thinning_factor <- 1
  } else {
    assert_scalar_size(thinning_factor, allow_zero = FALSE, call = call)
    if (n_steps %% thinning_factor != 0) {
      cli::cli_abort(
        "'thinning_factor' must be a divisor of 'n_steps'",
        call = call)
    }
  }
  ret <- list(total = n_steps,
              burnin = burnin,
              thinning_factor = thinning_factor)
  class(ret) <- "monty_sample_steps"
  ret
}


tail_and_pool <- function(pars, p, n) {
  n_samples <- ncol(pars)
  n_keep <- min(ceiling(n_samples * p), n)
  ret <- pars[, seq(to = n_samples, length.out = n_keep), , drop = FALSE]
  dim(ret) <- c(nrow(ret), prod(dim(ret)[-1]))
  ret
}
