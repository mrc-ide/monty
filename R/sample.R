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
##' @return A list of parameters and densities.
##'
##' @export
mcstate_sample <- function(model, sampler, n_steps, initial = NULL) {
  if (!inherits(model, "mcstate_model")) {
    cli::cli_abort("Expected 'model' to be an 'mcstate_model'",
                   arg = "model")
  }
  if (!inherits(sampler, "mcstate_sampler")) {
    cli::cli_abort("Expected 'sampler' to be an 'mcstate_sampler'",
                   arg = "sampler")
  }

  ## We might change this later.
  rng <- mcstate_rng$new()
  r_rng_state <- get_r_rng_state()

  if (is.null(initial)) {
    ## Really this would just be from the prior; we can't directly
    ## sample from the posterior!
    pars <- model$direct_sample(rng)
  } else {
    pars <- initial
    if (length(pars) != length(model$parameters)) {
      cli::cli_abort(
        paste("Unexpected initial parameter length {length(pars)};",
              "expected {length(model$parameters)}"),
        arg = "initial")
    }
  }

  density <- model$density(pars)
  state <- list(pars = pars, density = density)
  sampler$initialise(state, model, rng)

  history_pars <- matrix(NA_real_, n_steps + 1, length(pars))
  history_pars[1, ] <- pars
  history_density <- rep(NA_real_, n_steps + 1)
  history_density[[1]] <- density

  for (i in seq_len(n_steps)) {
    state <- sampler$step(state, model, rng)
    history_pars[i + 1, ] <- state$pars
    history_density[[i + 1]] <- state$density
  }

  ## Pop the parameter names on last
  colnames(history_pars) <- model$parameters

  ## I'm not sure about the best name for this
  details <- sampler$finalise(state, model, rng)

  if (!identical(get_r_rng_state(), r_rng_state)) {
    cli::cli_warn(c(
      "Detected use of R's random number generators",
      i = paste("Your model has used R's random number generators (e.g.,",
                "via rnorm, runif, sample, etc).  This means that your",
                "results will not be reproducible as you change the sample",
                "runner")))
  }

  list(pars = history_pars,
       density = history_density,
       details = details)
}


mcstate_sampler <- function(name, initialise, step, finalise) {
  ret <- list(name = name,
              initialise = initialise,
              step = step,
              finalise = finalise)
  class(ret) <- "mcstate_sampler"
  ret
}
