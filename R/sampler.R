##' A `monty_sampler2` object can be passed into `monty_sample` in
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
##' `monty_sampler2` but typically this will be a named list.  The
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
##' There are five state functions.  If you provide one, you probably
##' will need to provide them all.
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
##'   to return everything.
##'
##' State initialisation is handled by `initialise`; however, a
##' nontrivial return value from this function does not imply that
##' your sampler needs to worry about state.  If you can entirely
##' construct this from the current state of the chain and the control
##' parameters, then no state management is required.
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
##'   provide `state_dump`, too.  The arguments here, if provided,
##'   must be
##'
##'   * `state_chain`
##'   * `state_sampler`
##'   * `control`
##'   * `model`
##'
##' @param details Optionally, a function to tidy internal state to be
##'   saved at the end of the run.  If you provide this you almost
##'   certainly need to provide `state_dump` and `state_restore`.  The
##'   arguments here must be:
##'
##'   * `state_chain`
##'   * `state_sampler`
##'   * `control`
##'   * `model`
##'
##'   but we expect that only `state_sampler` will generally be
##'   needed.  Return whatever you fancy.
##'
##' @return A `monty_sampler2` object
##' @export
monty_sampler2 <- function(name, help, control, initialise, step,
                           state_dump = NULL, state_combine = NULL,
                           state_restore = NULL, state_details = NULL) {
  ## TODO: allow functions to be names and accept 'package' as an arg
  ## here, which will help with using the callr runner because we can
  ## organise loading packages and finding functions as required, even
  ## where the user has used a devtools-loaded package.
  assert_scalar_character(name)
  assert_scalar_character(help)

  state <- monty_sampler2_state(state_dump,
                                state_restore,
                                state_combine,
                                state_details)

  ret <- list(
    name = name,
    help = help,
    control = control,
    initialise = initialise,
    step = step,
    state = state)

  ## TODO: check that the functions are suitable?

  class(ret) <- "monty_sampler2"
  ret
}


##' @export
print.monty_sampler2 <- function(x, ...) {
  cli::cli_h1("<monty_sampler: {x$name} ({x$help})>")
  cli::cli_alert_info("Use {.help monty_sample} to use this sampler")
  cli::cli_alert_info("See {.help {x$help}} for more information")
  invisible(x)
}


## This is used in the compatibility code
is_v2_sampler <- function(sampler) {
  inherits(sampler, "monty_sampler2")
}


monty_sampler2_state <- function(dump, restore, combine, details) {
  if (is.null(dump)) {
    return(monty_sampler2_state_empty())
  }
  ret <- list(dump = dump,
              restore = restore,
              combine = combine,
              details = details %||% monty_sampler2_default_details)
  class(ret) <- "monty_sampler2_state"
  ret
}



## Most of this can probably come out
monty_sampler2_state_empty <- function() {
  monty_sampler2_state(
    function(...) NULL,
    function(...) NULL,
    function(...) NULL,
    function(...) NULL)
}


monty_sampler2_default_dump <- function(state_sampler) {
  if (is.null(state_sampler)) {
    state_sampler
  } else {
    stopifnot(is.environment(state_sampler))
    as.list(state_sampler, sorted = TRUE)
  }
}


monty_sampler2_default_restore <- function(chain_id, state_chain, state_sampler,
                                           control, model) {
  if (is.null(state_sampler)) {
    state_sampler
  } else {
    list2env(state_sampler, parent = emptyenv())
  }
}


monty_sampler2_default_details <- function(state) {
  return(NULL)
}


monty_sampler2_default_combine <- function(state) {
  if (all(vlapply(state, is.null))) {
    return(NULL)
  }
  browser()
}
