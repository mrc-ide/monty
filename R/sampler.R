## In a couple of PRs we'll be in a position to describe what is
## required here to write your own sampler.  However, there are still
## a few bits to add and remove, sowe don't have docs for this yet.
##
## The place we're trying to iterate to is for the PT sampler to be
## able to gracefully cope with the base chain being drawn from the
## prior (this will require that we can decompose 'step' into
## propose/evaluate/accept and allow us to override some of these, or
## that we rqeuire that every sampler to be able to swap in some
## directly sampled points).
##
## This might also be where we fix the multiple attempts at finite
## initial points?
##
## The first step towards this is making a bit more explicit the flow
## of data through the sampling process rather than having every
## sampler have some mysterious internal storage to move them to being
## stateless and shiting state management out into the runner which is
## shared between all samplers.
##
## Briefly we have (subject to change)
##
## name - a human readable name for the sampler
## help - the name of the constructor, used in constructing help text
## inputs - the (possibly lightly cleaned) inputs used to construct the sampler
## begin - initialise state
## step - update one step
## details - internal details for debugging, called after a series of steps
## internal_state - processing function called on internal state at end of chain
## resume - processing function called when resuming the chain
monty_sampler <- function(name, help, inputs, begin, step, details,
                          internal_state, resume) {
  ret <- list(name = name,
              help = help,
              inputs = inputs,
              begin = begin,
              step = step,
              details = details,
              internal_state = internal_state,
              resume = resume)
  class(ret) <- "monty_sampler"
  ret
}


## At this point we could use a data constructor for all the mutable
## state in running the model, I think.
monty_sampler_shared <- function(model, inputs, rng) {
  # initialise_rng_state(model, rng)
  list2env(
    list(model = model, rng = rng, inputs = inputs),
    parent = emptyenv())
}
