##' Create a simple Metropolis-Hastings sampler (really just a plain
##' Metropolis sampler since we assume symmetric proposals and don't
##' yet support the correction for asymmetric proposal).
##'
##' @title Metropolis-Hastings Sampler
##'
##' @param proposal A proposal function; must take a vector of
##'   parameters and produce a new vector of proposed parameters.
##'
##' @param vcv A variance covariance matrix to generate a `proposal`
##'   function from.  If you want multivariate Gaussian proposal, this
##'   is likely simpler than supplying your own `proposal`, and
##'   generally more efficient too.
##'
##' @return A `mcstate_sampler` object, which can be used with
##'   [mcstate_sample]
##'
##' @importFrom stats runif
##' @export
mcstate_sampler_metropolis_hastings <- function(proposal = NULL, vcv = NULL) {
  if (is.null(proposal) && is.null(vcv)) {
    cli::cli_abort("One of 'proposal' or 'vcv' must be given")
  }
  if (!is.null(proposal) && !is.null(vcv)) {
    cli::cli_abort("Only one of 'proposal' or 'vcv' may be given")
  }
  if (!is.null(vcv)) { # proposal is null
    check_vcv(vcv, call = environment())
    proposal <- make_rmvnorm(vcv)
  }

  initialise <- function(state, model) {
    if (!is.null(vcv)) {
      n_pars <- length(state$pars)
      n_vcv <- nrow(vcv)
      if (n_pars != n_vcv) {
        cli::cli_abort(
          "Incompatible length parameters ({n_pars}) and vcv ({n_vcv})")
      }
    }
  }

  step <- function(state, model) {
    density_accept <- state$density + log(runif(1))
    pars_next <- proposal(state$pars)

    ## We might want to do a few things here to pass in density_accept,
    ## and to indicate that we want to use the model (if provided) and
    ## not the prior here.  We might also want to report all of the
    ## three probabilities here, as at the moment we don't allow that
    ## decomposition.
    density_next <- model$density(pars_next)
    if (density_next > density_accept) {
      state$pars <- pars_next
      state$density <- density_next
    }
    state
  }

  finalise <- function(state, model) {
    NULL
  }

  mcstate_sampler("Metropolis-Hastings",
                  initialise,
                  step,
                  finalise)
}
