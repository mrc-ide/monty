##' Create a simple random walk sampler, which uses a symmetric
##' proposal to move around parameter space.  This sampler supports
##' sampling from models where the likelihood is only computable
##' randomly (e.g., for pmcmc).
##'
##' @title Random Walk Sampler
##'
##' @param vcv A variance covariance matrix for the proposal.
##'
##' @return A `mcstate_sampler` object, which can be used with
##'   [mcstate_sample]
##'
##' @export
mcstate_sampler_random_walk <- function(vcv = NULL) {
  check_vcv(vcv, call = environment())
  internal <- new.env()

  initialise <- function(pars, model, observer, rng) {
    n_pars <- length(model$parameters)
    n_vcv <- nrow(vcv)
    if (n_pars != n_vcv) {
      cli::cli_abort(
        "Incompatible length parameters ({n_pars}) and vcv ({n_vcv})")
    }
    internal$proposal <- make_rmvnorm(vcv)
    initialise_state(pars, model, observer, rng)
  }

  step <- function(state, model, observer, rng) {
    pars_next <- internal$proposal(state$pars, rng)
    density_next <- model$density(pars_next)

    if (density_next - state$density > log(rng$random_real(1))) {
      state <- update_state(state, pars_next, density_next,
                            model, observer, rng)
    }
    state
  }

  ## These are all effectively the defaults:
  finalise <- function(state, model, rng) {
    NULL
  }

  get_internal_state <- function() {
    NULL
  }

  set_internal_state <- function(state) {
  }

  mcstate_sampler("Random walk",
                  initialise,
                  step,
                  finalise,
                  get_internal_state,
                  set_internal_state)
}
