initialise_state <- function(pars, model, rng) {
  if (isTRUE(model$properties$is_stochastic)) {
    model$rng_state$set(mcstate_rng$new(rng$state())$jump()$state())
  }
  density <- model$density(pars)
  list(pars = pars, density = density)
}
