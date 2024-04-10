initialise_state <- function(pars, model, observer, rng) {
  initialise_rng_state(model, rng)
  density <- model$density(pars)
  if (is.null(observer)) {
    observation <- NULL
  } else {
    observation <- observer$observe(model$model, rng)
  }
  list(pars = pars, density = density, observation = observation)
}


update_state <- function(state, pars, density, model, observer, rng) {
  state$pars <- pars
  state$density <- density
  if (!is.null(observer)) {
    state$observation <- observer$observe(model$model, rng)
  }
  state
}


initialise_rng_state <- function(model, rng) {
  if (isTRUE(model$properties$is_stochastic)) {
    model$rng_state$set(mcstate_rng$new(rng$state())$jump()$state())
  }
}
