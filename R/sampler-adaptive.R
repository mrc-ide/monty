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
mcstate_sampler_adaptive <- function(initial_vcv,
                                     initial_scaling = 1,
                                     min_scaling = 0,
                                     scaling_increment = NULL,
                                     log_scaling_update = TRUE,
                                     acceptance_target = 0.234,
                                     initial_vcv_weight = 1000,
                                     forget_rate = 0.2,
                                     forget_end = Inf,
                                     adapt_end = Inf,
                                     pre_diminish = 0) {
  ## This sampler is stateful; we will be updating our estimate of the
  ## mean and vcv of the target distribution, along with the our
  ## scaling factor, weight and autocorrelations.
  ##
  ## Probably we will provide some method later for extracting this
  ## internal state and dump it out at the end of the sampling (so
  ## perhaps 'finalise') as I think we've previously collected this up
  ## at the end of the sampling as the estimated vcv is of interest.
  internal <- new.env()

  initialise <- function(state, model, rng) {
    internal$weight <- 0
    internal$iteration <- 0
    
    internal$mean <- state$pars
    d <- length(internal$mean)
    internal$autocorrelation <- matrix(0, d, d)
    
    internal$scaling <- initial_scaling
    internal$scaling_increment <- scaling_increment %||%
      calc_scaling_increment(length(internal$mean), acceptance_target)
    internal$n_start <- calc_n_start(acceptance_target)
    
    internal$history_pars <- c()
    internal$included <- c()
    internal$scaling_history <- internal$scaling
  }

  step <- function(state, model, rng) {
    vcv <- adaptive_vcv(internal$scaling, internal$autocorrelation,
                        internal$weight, internal$mean, initial_vcv,
                        initial_vcv_weight)
    
    pars_next <- rmvnorm(state$pars, vcv, rng)

    u <- rng$random_real(1)
    density_next <- model$density(pars_next)
    
    accept_prob <- min(1, exp(density_next - state$density))

    accept <- u < accept_prob
    if (accept) {
      state$pars <- pars_next
      state$density <- density_next
    }

    internal$iteration <- internal$iteration + 1
    internal$history_pars <- rbind(internal$history_pars, state$pars) 
    if (internal$iteration > adapt_end) {
      return(invisible())
    }
    is_replacement <- 
      check_replacement(internal$iteration, forget_rate, forget_end)
    if (is_replacement) {
      pars_remove <- internal$history_pars[internal$included[1], ]
    } else {
      pars_remove <- NULL
      internal$weight <- internal$weight + 1 
    }
    
    internal$scaling <- 
      update_scaling(internal$scaling, internal$iteration, accept_prob,
                     internal$scaling_increment, min_scaling, acceptance_target, 
                     pre_diminish, internal$n_start, log_scaling_update)
    internal$scaling_history <- c(internal$scaling_history, internal$scaling)
    internal$autocorrelation <- update_autocorrelation(
      state$pars, internal$weight, internal$autocorrelation, pars_remove)
    internal$mean <- update_mean(state$pars, internal$weight, internal$mean,
                                 pars_remove)
    internal$included <- 
      update_included(internal$included, internal$iteration, is_replacement)

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


calc_scaling_increment <- function(d, acceptance_target) {
  A <- - qnorm(acceptance_target / 2)
  
  (1 - 1 / d) * (sqrt(2 * pi) * exp(A ^ 2 / 2)) / (2 * A) + 
    1 / (d * acceptance_target * (1 - acceptance_target))
}


calc_n_start <- function(acceptance_target) {
  5 / (acceptance_target * (1 - acceptance_target))
}


qp <- function(x) {
  outer(x, x)
}


adaptive_vcv <- function(scaling, autocorrelation, weight, mean, initial_vcv,
                         initial_vcv_weight) {
  if (weight > 1) {
    vcv <- autocorrelation - weight / (weight - 1) * qp(mean)
  } else {
    vcv <- 0 * autocorrelation
  }
  
  d <- length(mean)
  
  weighted_vcv <-
    ((weight - 1) * vcv + (initial_vcv_weight + d + 1) * initial_vcv) /
    (weight + initial_vcv_weight + d + 1)
  
  2.38 ^ 2 / d * scaling ^ 2 * weighted_vcv
}


check_replacement <- function(iteration, forget_rate, forget_end) {
  is_forget_step <- floor(forget_rate * iteration) >
    floor(forget_rate * (iteration - 1))
  is_before_forget_end <- iteration <= forget_end
  
  is_forget_step & is_before_forget_end
}


update_scaling <- function(scaling, iteration, accept_prob, scaling_increment,
                           min_scaling, acceptance_target, pre_diminish, 
                           n_start, log_scaling_update) {
  scaling_change <- scaling_increment * (accept_prob - acceptance_target) /
    sqrt(n_start + max(0, iteration - pre_diminish))
  
  if (log_scaling_update) {
    scaling <- pmax(min_scaling, scaling * exp(scaling_change))
  } else {
    scaling <- pmax(min_scaling, scaling + scaling_change)
  }
  
}


update_autocorrelation <- function(pars, weight, autocorrelation, pars_remove) {
  if (!is.null(pars_remove)) {
    if (weight > 2) {
      autocorrelation <-
        autocorrelation + 1 / (weight - 1) * (qp(pars) - qp(pars_remove))
    } else {
      autocorrelation <- autocorrelation + qp(pars) - qp(pars_remove)
    }
  } else {
    if (weight > 2) {
      autocorrelation <-
        (1 - 1 / (weight - 1)) * autocorrelation + 1 / (weight - 1) * qp(pars)
    } else {
      autocorrelation <- autocorrelation + qp(pars)
    }
  }
  
  autocorrelation
}


update_mean <- function(pars, weight, mean, pars_remove) {
  if (!is.null(pars_remove)) {
    mean <- mean + 1 / weight * (pars - pars_remove)
  } else {
    mean <- (1 - 1 / weight) * mean + 1 / weight * pars
  }
  
  mean
}


update_included <- function(included, i, is_replacement) {
  if (is_replacement) {
    included <- c(included[-1L], i)
  } else {
    included <- c(included, i)
  }
  
  included
}
