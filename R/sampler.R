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
                           state_dump = NULL, state_restore = NULL,
                           details = NULL) {
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
                 restore = state_restore %||% monty_sampler2_default_restore),
    details = details)

  ## TODO: check that the functions are suitable?

  ## TODO: details should accept fewer arguments, but I am not sure
  ## which we can get away with dropping.  The state of the chain
  ## would be ideal.
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


monty_sampler2_default_dump <- function(state_sampler) {
  if (is.null(state_sampler)) {
    state_sampler
  } else {
    stopifnot(is.environment(state_sampler))
    as.list(state_sampler, sorted = TRUE)
  }
}


monty_sampler2_default_restore <- function(state_chain, state_sampler,
                                           control, model) {
  if (is.null(state_sampler)) {
    state_sampler
  } else {
    list2env(state_sampler, parent = emptyenv())
  }
}
