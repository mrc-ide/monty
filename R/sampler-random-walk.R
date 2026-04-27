##' Create a simple random walk sampler, which uses a symmetric
##' proposal to move around parameter space.  This sampler supports
##' sampling from models where the likelihood is only computable
##' randomly (e.g., for pmcmc).
##'
##' @title Random Walk Sampler
##'
##' @param vcv A variance covariance matrix for the proposal.
##'
##' @param boundaries Control the behaviour of proposals that are
##'   outside the model domain.  The supported options are:
##'
##'   * "reflect" (the default): we reflect proposed parameters that
##'     lie outside the domain back into the domain (as many times as
##'     needed)
##'
##'   * "reject": we do not evaluate the density function, and return
##'     `-Inf` for its density instead.
##'
##'   * "ignore": evaluate the point anyway, even if it lies outside
##'     the domain.
##'
##' The initial point selected will lie within the domain, as this is
##' enforced by [monty_sample].
##'
##' @param rerun_every Optional integer giving the frequency at which
##'   we should rerun the model on the current "accepted" parameters to
##'   obtain a new density for stochastic models.  The default for this
##'   (`Inf`) will never trigger a rerun, but if you set to 100, then
##'   every 100 steps we run the model on both the proposed *and* previously
##'   accepted parameters before doing the comparison.  This may help "unstick"
##'   chains, at the cost of some bias in the results.
##'
##' @param rerun_random Logical, controlling the behaviour of
##'   rerunning (when `rerun_every` is finite). With the default value of
##'   `TRUE`, we stochastically rerun at each step with probability of
##'   `1 / rerun_every`. If `FALSE` we rerun the model at fixed intervals of
##'   iterations (given by `rerun_every`). The two methods give the same
##'   expected number of MCMC steps between reruns but a different
##'   pattern.
##'
##' @return A `monty_sampler` object, which can be used with
##'   [monty_sample]
##'
##' @export
monty_sampler_random_walk <- function(vcv, boundaries = "reflect",
                                      rerun_every = Inf, rerun_random = TRUE) {
  check_vcv(vcv, allow_3d = TRUE, call = environment())
  boundaries <- match_value(boundaries, c("reflect", "reject", "ignore"))
  if (!identical(unname(rerun_every), Inf)) {
    assert_scalar_positive_integer(rerun_every)
  }
  assert_scalar_logical(rerun_random)
  control <- list(vcv = vcv,
                  boundaries = boundaries,
                  rerun = rerun_every < Inf,
                  rerun_every = rerun_every,
                  rerun_random = rerun_random)
  properties <- monty_sampler_properties(
    allow_augmented_data = TRUE,
    allow_multiple_parameters = TRUE)

  monty_sampler("Random walk",
                "monty_random_walk",
                control,
                sampler_random_walk_initialise,
                sampler_random_walk_step,
                sampler_random_walk_dump,
                sampler_random_walk_combine,
                sampler_random_walk_restore,
                properties = properties)
}


## The core sampler functions: initalise, step, dump, restore
sampler_random_walk_initialise <- function(state_chain, control, model, rng) {
  pars <- state_chain$pars
  ## We don't actually need to return an environment here, because all
  ## our things that are modified by reference (rerun) will already be
  ## captured in an environment
  list(proposal = make_random_walk_proposal(control, model, pars),
       rerun = make_rerun(control, model))
}


sampler_random_walk_step <- function(state_chain, state_sampler, control,
                                     model, rng) {
  if (control$rerun) {
    rerun <- state_sampler$rerun(rng)
    if (any(rerun)) {
      ## This is currently just setup assuming we are not using multiple
      ## parameters as currently they cannot be used with stochastic models,
      ## while the rerun is only used with stochastic models
      state_chain$density <- model$density(state_chain$pars)
    }
  }

  pars_next <- state_sampler$proposal(state_chain$pars, rng)
  reject_some <- control$boundaries == "reject" &&
    !all(i <- is_parameters_in_domain(pars_next, model$domain))
  if (reject_some) {
    density_next <- rep(-Inf, length(state_chain$density))
    if (any(i)) {
      density_next[i] <- model$density(pars_next[, i, drop = FALSE])
    }
  } else {
    density_next <- model$density(pars_next)
  }
  accept <- density_next - state_chain$density > log(monty_random_real(rng))

  update_state(state_chain, pars_next, density_next, accept, model)
}


sampler_random_walk_dump <- function(state, control) {
  if (is.null(state$rerun)) {
    return(NULL)
  }
  list(rerun_state = attr(state$rerun, "data")$i)
}


sampler_random_walk_combine <- function(state, control) {
  if (all(vlapply(state, is.null))) {
    return(NULL)
  }
  ## The only state that is saved is the rerun state, which is just a
  ## counter of the steps taken. That is the same over all chains so
  ## just get the first.
  state[[1]]
}


sampler_random_walk_restore <- function(chain_id, state_chain,
                                        state_sampler, control,
                                        model) {
  pars <- state_chain$pars
  list(proposal = make_random_walk_proposal(control, model, pars),
       rerun = make_rerun(control, model, state_sampler$rerun_state))
}


make_random_walk_proposal <- function(control, model, pars) {
  vcv <- sampler_validate_vcv(control$vcv, pars)
  make_random_walk_proposal_fn(vcv, model$domain, control$boundaries)
}
