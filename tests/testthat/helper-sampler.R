toy_sampler_initialise <- function(state_chain, control, model, rng) {
  n_sd <- length(control$sd)
  n_pars <- length(model$parameters)
  if (n_sd != n_pars) {
    cli::cli_abort("'control$sd' has length {n_sd} but your model has {n_pars} parameter{?s}")
  }
  NULL
}


toy_sampler_step <- function(state_chain, state_sampler, control, model, rng) {
  pars <- state_chain$pars
  density <- state_chain$density
  sd <- control$sd
  n_pars <- length(sd) # same as length(model$parameters)

  for (i in seq_len(n_pars)) {
    pars_next <- pars
    pars_next[[i]] <- monty_random_normal(pars[[i]], sd[[i]], rng)
    density_next <- monty_model_density(model, pars_next)
    accept <- density_next > density ||
      density_next - density > log(monty_random_real(rng))
    if (accept) {
      pars <- pars_next
      density <- density_next
    }
  }

  # Put state back together and return
  state_chain$pars <- pars
  state_chain$density <- density
  state_chain
}
