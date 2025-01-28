monty_sampler <- function(name, help, inputs, begin, step, details,
                          internal_state) {
  ret <- list(name = name,
              help = help,
              inputs = inputs,
              begin = begin,
              step = step,
              details = details,
              internal_state = internal_state)
  class(ret) <- "monty_sampler"
  ret
}


## At this point we could use a data constructor for all the mutable
## state in running the model, I think
monty_sampler_state <- function(model, inputs, rng) {
  ...
}


## Shared holds: inputs, state, model, rng (and name)
random_walk_begin <- function(shared, internal, pars, n_chains) {
  model <- shared$model
  inputs <- shared$inputs

  n_pars <- length(model$parameters)
  multiple_parameters <- length(dim2(pars)) > 1

  if (multiple_parameters) {
    stopifnot(n_pars == nrow(pars),
              n_chains == ncol(pars),
              model$properties$allow_multiple_parameters)
  } else {
    stopifnot(n_pars == length(pars))
  }

  ## This checks that our vcv is compatibile with the set of
  ## parameters we have been given
  vcv <- sampler_validate_vcv(inputs$vcv, pars)
  internal$proposal <-
    make_random_walk_proposal(vcv, model$domain, inputs$boundaries)

  internal$rerun <- make_rerun(inputs$rerun_every,
                               inputs$rerun_random,
                               model$properties$is_stochastic)

  ## TODO: refactor this later, or at least make it easy for other
  ## people to reuse.
  shared$state <- initialise_state(pars, model, shared$rng)
}


random_walk_step <- function(shared, internal) {
  rng <- shared$rng
  model <- shared$model

  pars_next <- internal$proposal(shared$state$pars, rng)
  rerun <- internal$rerun(rng)
  state <- shared$state
  if (any(rerun)) {
    ## This is currently just setup assuming we are not using multiple
    ## parameters as currently they cannot be used with stochastic models,
    ## while the rerun is only used with stochastic models
    state$density <- shared$model$density(state$pars)
  }

  reject_some <- shared$inputs$boundaries == "reject" &&
    !all(i <- is_parameters_in_domain(pars_next, model$domain))
  if (reject_some) {
    density_next <- rep(-Inf, length(state$density))
    if (any(i)) {
      ## TODO: this makes the assumption that we can pass a matrix
      ## with fewer parameter sets through to dust, but I think
      ## that requires some extra tweaks, as it won't really work
      ## with the ideas around index_group yet.
      density_next[i] <- model$density(pars_next[, i, drop = FALSE])
    }
  } else {
    density_next <- model$density(pars_next)
  }

  accept <- density_next - state$density > log(monty_random_real(rng))
  shared$state <- update_state(state, pars_next, density_next, accept,
                               model, rng)
}


random_walk_details <- function(shared, internal) {
  NULL
}


random_walk_internal_state <- function(shared, internal) {
  ## We might get the rerun offset and the proposal kernel here to
  ## make this easier to serialise.  Otherwise we should be a little
  ## careful about what those closures capture, but I think they're ok.
  list(rerun = internal$rerun, proposal = internal$proposal)
}


monty_sampler_random_walk <- function(vcv,
                                      boundaries = "reflect",
                                      rerun_every = Inf,
                                      rerun_random = TRUE) {
  check_vcv(vcv, allow_3d = TRUE)
  match_value(boundaries, c("reflect", "reject", "ignore"))
  if (!identical(unname(rerun_every), Inf)) {
    assert_scalar_positive_integer(rerun_every)
  }
  assert_scalar_logical(rerun_random)
  inputs <- list(vcv = vcv,
                 boundaries = boundaries,
                 rerun_every = rerun_every,
                 rerun_random = rerun_random)

  ## I've started breaking this up, but I think we need more hooks in
  ## here.  If we start sampling from the true "direct sample" density
  ## then we have to unconditionally accept the sample, that requires
  ## that we get more deeply involved in the internal calculations for
  ## density aned acceptance.
  monty_sampler("Random walk",
                "monty_sampler_random_walk",
                inputs,
                random_walk_begin,
                random_walk_step,
                random_walk_details,
                random_walk_internal_state)
}
