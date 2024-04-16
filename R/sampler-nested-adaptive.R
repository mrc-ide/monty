##' Create a nested random walk sampler, which uses a symmetric
##' proposal for separable sections of a model to move around in
##' parameter space.  This sampler supports sampling from models where
##' the likelihood is only computable randomly (e.g., for pmcmc), and
##' requires that models support the `has_parameter_groups` property.
##'
##' The intended use case for this sampler is for models where the
##' density can be decomposed at least partially into chunks that are
##' independent from each other.  Our motivating example for this is a
##' model of COVID-19 transmission where some parameters
##' region-specific (e.g., patterns and rates of contact between
##' individuals), and some parameters are shared across all regions
##' (e.g., intrinsic properties of the disease such as incubation
##' period).
##'
##' The strategy is to propose all the shared parameters as a
##' deviation from the current point in parameter space as a single
##' move and accept or reject as a block. Then we generate points for
##' all the region-specific parameters, compute the density and then
##' accept or reject these updates independently.  This is possible
##' because the change in likelihood in region A is independent from
##' region B.
##'
##' We expect that this approach will be beneficial in limited
##' situations, but where it is beneficial it is likely to result in
##' fairly large speedups:
##'
##' * You probably need more than three regions; as the number of
##'   regions increases the benefit independently accepting or
##'   rejecting densities increases (with 1000 separate regions your
##'   chains will mix very slowly for example).
##' * Your model is fairly comutationally heavy so that the density
##'   calculation completely dominates the sampling process.
##' * You do not have access to gradient information for your model;
##'   we suspect that HMC will outperform this approach by some margin
##'   because it already includes this independence via the gradients.
##' * You can compute your independent calculations in parallel, which
##'   help this method reduce your walk time.
##'
##' @title Nested Random Walk Sampler
##'
##' @param initial_vcv An initial variance covariance matrix; we'll start
##'   using this in the proposal, which will gradually become more weighted
##'   towards the empirical covariance matrix calculated from the chain.
##'
##' @param initial_vcv_weight Weight of the initial variance-covariance
##'   matrix used to build the proposal of the random-walk. Higher
##'   values translate into higher confidence of the initial
##'   variance-covariance matrix and means that update from additional
##'   samples will be slower.
##'
##' @param initial_scaling The initial scaling of the variance
##'   covariance matrix to be used to generate the multivariate normal
##'   proposal for the random-walk Metropolis-Hastings algorithm. To generate
##'   the proposal matrix, the weighted variance covariance matrix is
##'   multiplied by the scaling parameter squared times 2.38^2 / n_pars (where
##'   n_pars is the number of fitted parameters). Thus, in a Gaussian target
##'   parameter space, the optimal scaling will be around 1.
##'
##' @param initial_scaling_weight The initial weight used in the scaling update.
##'   The scaling weight will increase after the first `pre_diminish`
##'   iterations, and as the scaling weight increases the adaptation of the
##'   scaling diminishes. If `NULL` (the default) the value is
##'   5 / (acceptance_target * (1 - acceptance_target)).
##'
##' @param min_scaling The minimum scaling of the variance covariance
##'   matrix to be used to generate the multivariate normal proposal
##'   for the random-walk Metropolis-Hastings algorithm.
##'
##' @param scaling_increment The scaling increment which is added or
##'   subtracted to the scaling factor of the variance-covariance
##'   after each adaptive step. If `NULL` (the default) then an optimal
##'   value will be calculated.
##'
##' @param log_scaling_update Logical, whether or not changes to the
##'   scaling parameter are made on the log-scale.
##'
##' @param acceptance_target The target for the fraction of proposals
##'   that should be accepted (optimally) for the adaptive part of the
##'   mixture model.
##'
##' @param forget_rate The rate of forgetting early parameter sets from the
##'   empirical variance-covariance matrix in the MCMC chains. For example,
##'   `forget_rate = 0.2` (the default) means that once in every 5th iterations
##'   we remove the earliest parameter set included, so would remove the 1st
##'   parameter set on the 5th update, the 2nd on the 10th update, and so
##'   on. Setting `forget_rate = 0` means early parameter sets are never
##'   forgotten.
##'
##' @param forget_end The final iteration at which early parameter sets can
##'   be forgotten. Setting `forget_rate = Inf` (the default) means that the
##'   forgetting mechanism continues throughout the chains. Forgetting early
##'   parameter sets becomes less useful once the chains have settled into the
##'   posterior mode, so this parameter might be set as an estimate of how long
##'   that would take.
##'
##' @param adapt_end The final iteration at which we can adapt the multivariate
##'   normal proposal. Thereafter the empirical variance-covariance matrix, its
##'   scaling and its weight remain fixed. This allows the adaptation to be
##'   switched off at a certain point to help ensure convergence of the chain.
##'
##' @param pre_diminish The number of updates before adaptation of the scaling
##'   parameter starts to diminish. Setting `pre_diminish = 0` means there is
##'   diminishing adaptation of the scaling parameter from the offset, while
##'   `pre_diminish = Inf` would mean there is never diminishing adaptation.
##'   Diminishing adaptation should help the scaling parameter to converge
##'   better, but while the chains find the location and scale of the posterior
##'   mode it might be useful to explore with it switched off.
##'
##' @return A `mcstate_sampler` object, which can be used with
##'   [mcstate_sample]
##'
##' @export
mcstate_sampler_nested_adaptive <- function(initial_vcv,
                                            initial_vcv_weight = 1000,
                                            initial_scaling = 1,
                                            initial_scaling_weight = NULL,
                                            min_scaling = 0,
                                            scaling_increment = NULL,
                                            log_scaling_update = TRUE,
                                            acceptance_target = 0.234,
                                            forget_rate = 0.2,
                                            forget_end = Inf,
                                            adapt_end = Inf,
                                            pre_diminish = 0) {
  if (!is.list(initial_vcv)) {
    cli::cli_abort(
      "Expected a list for 'initial_vcv'",
      arg = 'initial_vcv')
  }

  if (!setequal(names(initial_vcv), c("base", "groups"))) {
    cli::cli_abort("Expected 'initial_vcv' to have elements 'base' and 'groups'",
                   arg = "initial_vcv")
  }
  if (!is.null(initial_vcv$base)) {
    check_vcv(initial_vcv$base, call = environment())
  }
  if (!is.list(initial_vcv$groups)) {
    cli::cli_abort("Expected 'initial_vcv$groups' to be a list")
  }
  if (length(initial_vcv$groups) < 1) {
    cli::cli_abort("Expected 'initial_vcv$groups' to have at least one element")
  }
  for (i in seq_along(initial_vcv$groups)) {
    check_vcv(initial_vcv$groups[[i]], name = sprintf("initial_vcv$groups[%d]", i),
              call = environment())
  }

  internal <- new.env(parent = emptyenv())

  initialise <- function(pars, model, observer, rng) {
    require_deterministic(model,
                          "Can't use adaptive sampler with stochastic models")
    
    if (!model$properties$has_parameter_groups) {
      cli::cli_abort("Your model does not have parameter groupings")
    }

    initialise_rng_state(model, rng)
    density <- model$density(pars, by_group = TRUE)
    density_by_group <- attr(density, "by_group")
    i_base <- model$parameter_groups == 0
    n_base <- sum(i_base)
    n_groups <- max(model$parameter_groups)
    i_group <- 
      lapply(seq_len(n_groups), function(i) which(model$parameter_groups == i))

    if (is.null(density_by_group)) {
      cli::cli_abort(
        c(paste("model$density(x, by_group = TRUE) did not produce a",
                "density with a 'by_group' attribute"),
          i = paste("I expected an attribute 'by_group' with {n_groups}",
                    "elements corresponding to parameter groups to be",
                    "included with your density")))
    }
    if (length(density_by_group) != n_groups) {
        cli::cli_abort(
          paste("model$density(x, by_group = TRUE) produced a 'by_group'",
                "attribute with incorrect length {length(density_by_group)}",
                "but I expected length {n_groups}"))
    }

    internal$density_by_group <- density_by_group
    
    internal$weight <- 0
    internal$iteration <- 0
    browser()
    internal$mean <- 
      list(base = pars[i_base],
           groups = lapply(seq_len(n_groups), function(i) pars[i_group[[i]]]))
    internal$autocorrelation <- 
      list(base = matrix(0, n_base, n_base),
           groups = lapply(lengths(i_group), function (x) matrix(0, x, x)))
    internal$vcv <- 
      list(base = update_vcv(internal$mean$base, internal$autocorrelation$base,
                             internal$weight),
           groups = Map(update_vcv, internal$mean$groups, 
                        internal$autocorrelation$groups, internal$weight))
    
    
    internal$scaling <- list(base = initial_scaling,
                             groups = rep(list(initial_scaling), n_groups))
    internal$scaling_increment <- 
      list(base = scaling_increment %||% 
             calc_scaling_increment(n_base, acceptance_target, 
                                    log_scaling_update),
           groups = lapply(lengths(i_group), 
                           function(x) {scaling_increment %||%
                               calc_scaling_increment(x, acceptance_target,
                                                      log_scaling_update)}))
    internal$scaling_weight <- initial_scaling_weight %||%
      5 / (acceptance_target * (1 - acceptance_target))
    
    proposal_vcv <- 
      list(base = calc_proposal_vcv(internal$scaling$base, internal$vcv$base,
                                    internal$weight, initial_vcv$base,
                                    initial_vcv_weight),
           groups = Map(calc_proposal_vcv, internal$scaling$groups,
                        internal$vcv$groups, internal$weight, 
                        initial_vcv$groups, initial_vcv_weight))
    internal$proposal <- nested_proposal(proposal_vcv, model$parameter_groups)
    
    internal$history_pars <- numeric()
    internal$included <- integer()
    internal$scaling_history <- internal$scaling
    
    state <- list(pars = pars, density = c(density))
    if (!is.null(observer)) {
      state$observation <- observer$observe(model$model, rng)
    }
    state
  }

  ## There are probably different modes that this could run in, they'd
  ## be fairly easy to change.  This one would correspond to some sort
  ## of "full update" mode where everything is done within a step, but
  ## we could also do one where we allow for picking one update type
  ## with some schedule or probability and applying that, which would
  ## allow for faster movement of some part of the chain.  We could
  ## handle this by additional arguments to the constructor, then
  ## either changing the behaviour of the step function or swapping in
  ## a different version.
  step <- function(state, model, observer, rng) {
    browser()
    if (!is.null(internal$proposal$base)) {
      pars_next <- internal$proposal$base(state$pars, rng)
      density_next <- model$density(pars_next, by_group = TRUE)
      density_by_group_next <- attr(density_next, "by_group")
      accept <- density_next - state$density > log(rng$random_real(1))
      if (accept) {
        state$pars <- pars_next
        state$density <- density_next
        internal$density_by_group <- density_by_group_next
        if (!is.null(observer)) {
          state$observation <- observer$observe(model$model, rng)
        }
      }
    }

    pars_next <- internal$proposal$groups(state$pars, rng)
    density_next <- model$density(pars_next, by_group = TRUE)
    density_by_group_next <- attr(density_next, "by_group")
    accept <- density_by_group_next - internal$density_by_group >
      log(rng$random_real(length(density_by_group_next)))

    if (any(accept)) {
      if (!all(accept)) {
        ## Retain some older parameters
        i <- model$parameter_groups %in% which(!accept)
        pars_next[i] <- state$pars[i]
        density_next <- model$density(pars_next, by_group = TRUE)
        density_by_group_next <- attr(density_next, "by_group")
      }
      state$pars <- pars_next
      state$density <- c(density_next)
      internal$density_by_group <- density_by_group_next
      if (!is.null(observer)) {
        state$observation <- observer$observe(model$model, rng)
      }
    }
    state
  }

  finalise <- function(state, model, rng) {
    NULL
  }

  get_internal_state <- function() {
    as.list(internal)
  }

  set_internal_state <- function(state) {
    list2env(state, internal)
  }

  mcstate_sampler("Nested random walk",
                  initialise,
                  step,
                  finalise,
                  get_internal_state,
                  set_internal_state)
}

