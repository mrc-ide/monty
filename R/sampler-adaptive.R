##' Create an adaptive Metropolis-Hastings sampler, which will tune
##' its variance covariance matrix (vs the simple random walk
##' sampler [mcstate_sampler_random_walk]).
##'
##' Efficient exploration of the parameter space during an MCMC might
##' be difficult when the target distribution is of high
##' dimensionality, especially if the target probability distribution
##' present a high degree of correlation.  Adaptive schemes are used
##' to "learn" on the fly the correlation structure by updating the
##' proposal distribution by recalculating the empirical
##' variance-covariance matrix and rescale it at each adaptive step of
##' the MCMC.
##'
##' Our implementation of an adaptive MCMC algorithm is based on an
##' adaptation of the "BlkAdpMul" algorithm in Sherlock et
##' al. (ALGORITHM 6). The algorithm is based on a random-walk
##' Metropolis-Hasting algorithm where the proposal is a multi-variate
##' Normal distribution centered on the current point.
##'
##' Sherlock C, Fearnhead P, Roberts GO (2010) The Random Walk
##' Metropolis: Linking Theory and Practice Through a Case
##' Study. Statistical Science 25:172–190.
##'
##' @title Adaptive Metropolis-Hastings Sampler
##'
##' @param vcv An initial variance covariance matrix; we'll start from
##'   this point and adapt the matrix over steps towards something
##'   that more closely resembles the covariance matrix of your
##'   distribution.
##'
##' @param initial_scaling The initial scaling of the variance
##'   covariance matrix to be used to generate the multivariate normal
##'   proposal for the random-walk Metropolis-Hasting algorithm.
##'
##' @param scaling_increment The scaling increment which is added or
##'   substracted to the scaling factor of the variance-covariance
##'   after each adaptive step.
##'
##' @param acceptance_target The target for the fraction of proposals
##'   that should be accepted (optimally) for the adaptive part of the
##'   mixture model.
##'
##' @param initial_weight Initial weight of the variance-covariance
##'   matrix used to build the proposal of the random-walk. Higher
##'   values translate into higher confidence of the initial
##'   variance-covariance matrix and means that update from additional
##'   samples will be slower.
##'
##' @param adaptive_contribution The fractional contribution of the
##'   adaptive part of the proposal. The proposal is based on a
##'   mixture model, with the non adaptive part used for the proposal
##'   with a probability 1-adaptive_contribution.
##'
##' @return A `mcstate_sampler` object, which can be used with
##'   [mcstate_sample]
##'
##' @export
mcstate_sampler_adaptive <- function(vcv,
                                     initial_scaling = 0.2,
                                     scaling_increment = 0.01,
                                     acceptance_target = 0.234,
                                     initial_weight = 1000,
                                     adaptive_contribution = 0.95) {
  ## This sampler is stateful; we will be updating our estimate of the
  ## mean and vcv of the target distribution, along with the our
  ## scaling factor, weight and autocorrelations.
  ##
  ## Probably we will provide some method later for extracting this
  ## internal state and dump it out at the end of the sampling (so
  ## perhaps 'finalise') as I think we've previously collected this up
  ## at the end of the sampling as the estimated vcv is of interest.
  internal <- new.env()

  qp <- function(x) {
    outer(x, x)
  }

  initialise <- function(state, model, rng) {
    internal$mean <- state$pars
    internal$vcv <- vcv

    internal$scaling <- initial_scaling
    internal$weight <- initial_weight
    internal$autocorrelation <- internal$vcv +
      internal$weight / (internal$weight - 1) * qp(internal$mean)
  }

  step <- function(state, model, rng) {
    is_adaptive <- rng$random_real(1) < adaptive_contribution
    if (is_adaptive) {
      internal$vcv <- internal$scaling *
        (internal$autocorrelation - internal$weight /
         (internal$weight - 1) * qp(internal$mean))
      pars_next <- rmvnorm(state$pars, internal$vcv)
    } else {
      pars_next <- rmvnorm(state$pars, vcv) # this is the initial vcv
    }

    u <- rng$random_real(1)
    density_accept <- state$density + log(u)
    density_next <- model$density(pars_next)

    accept <- density_next > density_accept
    if (accept) {
      state$pars <- pars_next
      state$density <- density_next
    }

    if (is_adaptive) {
      if (accept) {
        internal$scaling <- internal$scaling +
          (1 - acceptance_target) * scaling_increment
      } else {
        internal$scaling <- max(
          internal$scaling - acceptance_target * scaling_increment,
          scaling_increment)
      }

      ## Update of the autocorrelation matrix and mean of past samples
      internal$weight <- internal$weight + 1
      internal$autocorrelation <-
        (1 - 1 / (internal$weight - 1)) * internal$autocorrelation +
        1 / (internal$weight - 1) * qp(state$pars)
      internal$mean <- (1 - 1 / internal$weight) * internal$mean +
        1 / internal$weight * state$pars
    }

    state
  }

  ## Marc; do we want all of these saved out?  Did we ever decide how
  ## to merge these over multiple chains?
  finalise <- function(state, model, rng) {
    as.list(internal)
  }

  mcstate_sampler("Adaptive Metropolis-Hastings",
                  initialise,
                  step,
                  finalise)
}
