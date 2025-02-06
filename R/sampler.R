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
