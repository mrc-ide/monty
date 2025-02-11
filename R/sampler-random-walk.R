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
                random_walk_internal_state,
                random_walk_resume)
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

  initialise_rng_state(shared$model, shared$rng)
  shared$pars <- pars
  shared$density <- model$density(pars)
  if (model$properties$has_observer) {
    shared$observation <- model$observer$observe()
  }

  ## TODO: refactor this later, or at least make it easy for other
  ## people to reuse.
  ## shared$state <- initialise_state(pars, model, shared$rng)
}


random_walk_resume <- function(shared, internal) {
}


random_walk_step <- function(shared, internal) {
  rng <- shared$rng
  model <- shared$model

  pars_next <- internal$proposal(shared$pars, rng)
  rerun <- internal$rerun(rng)
  if (any(rerun)) {
    ## This is currently just setup assuming we are not using multiple
    ## parameters as currently they cannot be used with stochastic models,
    ## while the rerun is only used with stochastic models
    shared$density <- shared$model$density(shared$pars)
  }

  reject_some <- shared$inputs$boundaries == "reject" &&
    !all(i <- is_parameters_in_domain(pars_next, model$domain))
  if (reject_some) {
    density_next <- rep(-Inf, length(shared$density))
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

  accept <- density_next - shared$density > log(monty_random_real(rng))
  shared$state <- update_state(shared, pars_next, density_next, accept)
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


make_random_walk_proposal <- function(vcv, domain, boundaries) {
  mvn <- make_rmvnorm(vcv)
  if (boundaries != "reflect" || !any(is.finite(domain))) {
    return(function(x, rng) {
      x + mvn(rng)
    })
  }

  x_min <- domain[, 1]
  x_max <- domain[, 2]
  function(x, rng) {
    reflect_proposal(x + mvn(rng), x_min, x_max)
  }
}


## create function to reflect proposal boundaries at pars_min and pars_max
## this ensures the proposal is symmetrical and we can simplify the MH step
reflect_proposal <- function(x, x_min, x_max) {
  i <- x < x_min | x > x_max
  if (any(i)) {
    i_both <- i & is.finite(x_min) & is.finite(x_max)
    i_min <- i & is.finite(x_min) & !is.finite(x_max)
    i_max <- i & !is.finite(x_min) & is.finite(x_max)
    if (is.matrix(x)) {
      ## This will be a bit different if the model supports array
      ## specifications of the domain, which we might want to support
      ## later I guess. Replicating these helps tidy up the
      ## bookkeeping, and also means we don't have to think about this
      ## very much.
      x_min <- array(x_min, dim(x))
      x_max <- array(x_max, dim(x))
    }
    x[i_both] <- reflect_proposal_both(x[i_both], x_min[i_both], x_max[i_both])
    x[i_min] <- reflect_proposal_one(x[i_min], x_min[i_min])
    x[i_max] <- reflect_proposal_one(x[i_max], x_max[i_max])
  }
  x
}


reflect_proposal_both <- function(x, x_min, x_max) {
  x_r <- x_max - x_min
  abs((x + x_r - x_min) %% (2 * x_r) - x_r) + x_min
}


reflect_proposal_one <- function(x, x_bound) {
  2 * x_bound - x
}


is_parameters_in_domain <- function(x, domain) {
  x_min <- domain[, 1]
  x_max <- domain[, 2]
  i <- x > x_min & x < x_max
  if (all(i)) {
    if (is.matrix(x)) rep(TRUE, ncol(x)) else TRUE
  } else {
    if (is.matrix(x)) apply(i, 2, all) else FALSE
  }
}


make_rerun <- function(every, random, is_stochastic) {
  if (!is_stochastic && !is.finite(every)) {
    function(rng) rep(FALSE, length(rng))
  } else if (random) {
    function(rng) monty_random_real(rng) < 1 / every
  } else {
    i <- 0L
    env <- environment()
    function(rng) {
      env$i <- i + 1L
      rep(i %% every == 0, length(rng))
    }
  }
}
