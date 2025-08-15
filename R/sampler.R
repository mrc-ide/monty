##' A `monty_sampler` object can be passed into `monty_sample` in
##' order to draw samples from a distribution.  The primary role of a
##' sampler is to advance the state of a Markov chain one step; in
##' doing so they may mutate some internal state (outside the
##' knowledge of the problem being advanced).  Ordinarily users will
##' not call this function, but authors of samplers will call it from
##' the constructor of their sampler.
##'
##' See `vignette("writing-samplers")` for an introduction to writing
##' samplers.
##'
##' Control parameters are used to build the sampler.  These are
##' immutable after creation.  The format is unspecified by
##' `monty_sampler` but typically this will be a named list.  The
##' sampler designer will construct this list and should take care not
##' to include anything mutable (e.g. environments) or hard to
##' serialise and transfer to another process here.
##'
##' # Sampler state
##'
##' Your sampler can have internal state.  This is not needed for
##' simple samplers; a random walk Metropolis-Hastings sampler does
##' not need this for example as its state is entirely defined by the
##' (`pars`, `density`) pair that forms the chain state.  Similarly,
##' simple implemenations of HMC or Gibbs samplers would not need this
##' functionality.  If you wish to record debug information, or if
##' your sampler updates some internal state as it runs -- as our
##' adaptive Metropolis-Hastings sampler does -- then you will need to
##' configure your sampler to initialise, store, combine and restore
##' this state.
##'
##' There are four state handling functions.  If you provide one, you
##' probably will need to provide them all, though `details` can be
##' omitted if you do not want to render out a user-facing summary of
##' your sampler state at the end of a chain.
##'
##' * `state_dump`: this takes the sampler state (which is often an
##'   environment) and returns a list.  In most cases, this is for a
##'   single chain, but if your sampler is used with
##'   [monty_runner_simultaneous()] this will correspond to the state
##'   for a number of chains at once, in which case your dumped state
##'   should look like the output from combining chains.
##'
##' * `state_combine`: this takes a list of sampler states, each of
##'   which was dumped with `state_dump` and combines them into a
##'   single state object.  You need to aim for the case where the
##'   output of this function is the same as running `state_dump`
##'   after running with [monty_runner_simultaneous()].  Hopefully we
##'   can write some things to help with this, or at least example
##'   tests that will probably satisfy this.
##'
##' * `state_restore`: this takes the output of `state_split` (or
##'   `state_combine` in the case of [monty_runner_simultaneous()])
##'   and prepares the state for use with the sampler.  This function
##'   takes arguments:
##'
##'   * `chain_id`: one or more chain ids
##'   * `state_chain`: the state of the chain(s) at the point of
##'     restoration
##'   * `state_sampler`: the state from `state_combine`
##'   * `control`: the sampler control
##'   * `model`: the model
##'
##' * `state_details`: this takes the output of `state_combine` and
##'   returns a cleaned version of the state back to the user, as the
##'   `$details` element of the final samples.  Use this to extract a
##'   fraction of the total state where only some should be
##'   user-visible.  You do not need to provide this, the default is
##'   to return nothing.
##'
##' State initialisation is handled by `initialise`; however, a
##' nontrivial return value from this function does not imply that
##' your sampler needs to worry very much about state.  If you can
##' entirely construct this from the current state of the chain and
##' the control parameters, then no state management is required.
##' However, a nontrivial return value from `initialise` requires a
##' `state_restore` argument, even if `state_dump` is not present.
##'
##' The state manoeuvres may feel tedious, but it will form part of
##' the core of how the Parallel Tempering algorithm works, where we
##' need to be ablt to run multiple chains through a sampler at the
##' same time.
##'
##' We will set things up soon so that if you do not provide these
##' functions (but if you do provide state), then your sampler will
##' work, but it will fail informatively when you try and continue it.
##'
##' @title Create a monty sampler
##'
##' @param name Name of the sampler. Usually this is the name of the
##'   algorithm and can include spaces and punctuation if desired.
##'
##' @param help Name of the function to direct users to find help.
##'   Usually that is the name of the constructor function.
##'
##' @param control A list of control parameters passed to the sampler.
##'   These are immutable (i.e., once created they cannot be changed).
##'
##' @param initialise A function to initialise the sampler.  This is
##'   called once at the start of the chain to set up any internal
##'   state, though in some cases it will not need to do very much.
##'   It must take arguments:
##'
##'   * `state_chain`: the state of the MCMC chain, containing
##'     elements `pars` (a vector or matrix of parameters), `density`
##'     and possibly `observation`
##'   * `control`: the control parameters, as originally passed to
##'     `monty_sampler`
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
##'   state for serialisation.  If not given, we assume that nothing
##'   needs to be saved and that your sampler can be restarted from
##'   just the state of the chain, the model and `control`.  If
##'   provided then typically you will need to provide
##'   `state_restore`, too.
##'
##' @param state_combine Optionally, a function to combine the output
##'   of several chains (a list, where each element has come from
##'   `state_dump`) into a single object that is consistent with what
##'   the simultaneous runner would have produced.
##'
##' @param state_restore Optionally, a function to take a dumped chain
##'   state and convert it back into an environment.  If not given, we
##'   assume that `list2env(x, parent = emptyenv())` is sufficient and
##'   use that (unless your state is `NULL`, in which case we use
##'   `identity`).  If provided then typically you will need to
##'   provide `state_dump`, too.  The arguments here, if provided,
##'   must be
##'
##'   * `chain_id`
##'   * `state_chain`
##'   * `state_sampler`
##'   * `control`
##'   * `model`
##'
##' @param state_details Optionally, a function to tidy internal state
##'   to be saved at the end of the run.  If you provide this you
##'   almost certainly need to provide `state_dump` and
##'   `state_restore`.  This takes the combined state as its only
##'   argument.
##'
##' @param properties Optionally, a [monty_sampler_properties] object
##'   that advertises what your sampler can do and what it requires
##'   from models that it draws samples from.
##'
##' @return A `monty_sampler` object
##' @export
monty_sampler <- function(name, help, control, initialise, step,
                          state_dump = NULL, state_combine = NULL,
                          state_restore = NULL, state_details = NULL,
                          properties = NULL) {
  properties <- validate_sampler_properties(properties,
                                            state_dump, state_restore,
                                            call)

  ## TODO: allow functions to be names and accept 'package' as an arg
  ## here, which will help with using the callr runner because we can
  ## organise loading packages and finding functions as required, even
  ## where the user has used a devtools-loaded package.
  assert_scalar_character(name)
  assert_scalar_character(help)

  state <- monty_sampler_state(state_dump,
                               state_combine,
                               state_restore,
                               state_details)

  ret <- list(
    name = name,
    help = help,
    control = control,
    initialise = initialise,
    step = step,
    state = state,
    properties = properties)

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


##' Describe properties of a sampler.  This is used from
##' [monty_sampler] to advertise what your sampler does about state,
##' what it requires from the runner and from the model, so that monty
##' can ensure that it is only used where it is appropriate.
##'
##' @title Describe sampler properties
##'
##' @param has_state Optional logical, indicating if the sampler has
##'   state.  This is optional because presence of the state function
##'   `state_dump` implies this.
##'
##' @param restartable Optional logical, indicating if your sampler
##'   can be restarted.  If `FALSE`, then users cannot use
##'   `restartable = TRUE` from `monty_sample()` (and therefore cannot
##'   use `monty_continue`).  This is optional because the presence of
##'   the state function `state_restore` implies this.
##'
##' @param allow_multiple_parameters Logical, indicating if your
##'   sampler can accept a matrix of parameters in order to run
##'   multiple chains at once (e.g., with the
##'   [monty_runner_simultaneous] runner, or as part of a parallel
##'   tempering scheme with [monty_sampler_parallel_tempering]).
##'
##' @param requires_gradient Logical, indicating if the model must
##'   provide a gradient in order to be used with this sampler.
##'
##' @param requires_allow_multiple_parameters Logical, indicating if
##'   the model must be able to accept multiple parameters.  This is
##'   different to `allow_multiple_parameters`, which concerns if the
##'   *sampler* is able to process multiple parameter sets at once.
##'   For example, [monty_sampler_parallel_tempering] sets
##'   `allow_multiple_parameters` to `FALSE` but
##'   `requires_allow_multiple_parameters` to `TRUE`, while
##'   [monty_sampler_random_walk] sets the opposite!
##'
##' @param requires_deterministic Logical, indicating if the model
##'   must be deterministic in order to be used with this sampler.
##'
##' @return A `monty_sampler_properties` object, which should not be
##'   modified.
##'
##' @export
##' @examples
##' monty_sampler_properties()
monty_sampler_properties <- function(has_state = NULL,
                                     restartable = NULL,
                                     allow_multiple_parameters = FALSE,
                                     requires_gradient = FALSE,
                                     requires_allow_multiple_parameters = FALSE,
                                     requires_deterministic = FALSE) {
  assert_scalar_logical(has_state, allow_null = TRUE)
  assert_scalar_logical(restartable, allow_null = TRUE)
  assert_scalar_logical(allow_multiple_parameters)
  assert_scalar_logical(requires_gradient)
  assert_scalar_logical(requires_allow_multiple_parameters)
  assert_scalar_logical(requires_deterministic)
  ret <- list(
    restartable = restartable,
    has_state = has_state,
    allow_multiple_parameters = allow_multiple_parameters,
    requires_gradient = requires_gradient,
    requires_allow_multiple_parameters = requires_allow_multiple_parameters,
    requires_deterministic = requires_deterministic)
  class(ret) <- "monty_sampler_properties"
  ret
}


##' @export
print.monty_sampler_properties <- function(x, ...) {
  cli::cli_h1("<monty_sampler_properties>")
  unset <- vlapply(x, is.null)
  is_set <- !unset
  if (any(is_set)) {
    cli::cli_bullets(
      set_names(sprintf("%s: {.code %s}",
                        names(x)[is_set], vcapply(x[is_set], as.character)),
                "*"))
  }
  if (any(unset)) {
    cli::cli_alert_info("Unset: {squote(names(x)[unset])}")
  }
  invisible(x)
}


monty_sampler_state <- function(dump, combine, restore, details,
                                call = parent.frame()) {
  if (is.null(dump)) {
    err <- c(state_combine = !is.null(combine),
             state_details = !is.null(details))
    if (any(err)) {
      cli::cli_abort(
        c(paste("Unexpected state handling function{?s} provided:",
                "{squote(names(which(err)))}"),
          i = "'state_dump' was 'NULL', so this is a stateless sampler"),
        call = call)
    }
    dump <- return_null
    combine <- return_null
    details <- return_null
    restore <- restore %||% return_null
  } else {
    err <- c(state_restore = !is.function(restore),
             state_combine = !is.function(combine))
    if (any(err)) {
      cli::cli_abort(
        c(paste("Missing state handling function{?s}:",
                "{squote(names(which(err)))}"),
          i = "'state_dump' was a function, so this is a stateful sampler"),
        call = call)
    }
  }
  list(dump = dump,
       restore = restore,
       combine = combine,
       details = details %||% return_null)
}


validate_sampler_properties <- function(properties, state_dump, state_restore,
                                        call = parent.frame()) {
  if (is.null(properties)) {
    properties <- monty_sampler_properties()
  } else {
    assert_is(properties, "monty_sampler_properties", call = call)
  }

  if (is.null(properties$has_state)) {
    properties$has_state <- !is.null(state_dump)
  } else if (properties$has_state && is.null(state_dump)) {
    cli::cli_abort(
      paste("A 'state_dump' function is required because sampler properties",
            "include 'has_state = TRUE'"),
      call = call)
  }

  if (is.null(properties$restartable)) {
    properties$restartable <- !is.null(state_restore)
  } else if (properties$restartable && is.null(state_restore)) {
    cli::cli_abort(
      paste("A 'state_restore' function is required because sampler properties",
            "include 'restartable = TRUE'"),
      call = call)
  }

  properties
}


check_sampler_model <- function(model, sampler, name = "model") {
  require_deterministic(
    model,
    sprintf("%s requires deterministic models, but '%s' is stochastic",
            sampler$name, name),
    when = sampler$properties$requires_deterministic)
  require_gradient(
    model,
    sprintf(
      "%s requires a gradient but '%s' does not provide one",
      sampler$name, name),
    when = sampler$properties$requires_gradient)
  require_multiple_parameters(
    model,
    sprintf(
      "%s requires multiple parameters at once but '%s' does not allow this",
      sampler$name, name),
    when = sampler$properties$requires_allow_multiple_parameters)
}
