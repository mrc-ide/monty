##' Create a "parallel tempering" sampler, which runs multiple chains
##' at once to try and improve mixing, or takes advantage of
##' vectorisation/parallelisation if your underlying model supports
##' it.  We have tested the implementation with the random walk
##' sampler ([monty_sampler_random_walk]) but this may work with other
##' samplers.
##'
##' We implement the sampler based on <https://doi.org/10.1111/rssb.12464>
##'
##' # Efficiency of the sampler
##'
##' A parallel tempering sampler runs a series of chains at the same
##' time, so is doing much more work than a simpler sampler.  If you
##' run with `n_rungs = 10` you are doing 11x more work than the
##' underlying base sampler, so you want to make sure that this is
##' paid back somewhere.  There are a few places where this efficiency
##' may come from:
##'
##' 1. **Your model is parallelisable**.  If your underlying model can
##' run very efficiently in parallel then it may not take much longer
##' in "wall time" to run the extra copies of the calculations.  In
##' this case, you'll still be using much more CPU time but will be
##' able to take advantage of extra cores to get more effective
##' sampling if the parallel tempering sampler mixes better than the
##' underlying sampler.
##'
##' 2. **Your model is vectorised**.  If your model is implemented in
##' R and vectorises the density calculations then it will generally
##' not take much longer to compute many densities at once than a
##' single one.
##'
##' 3. **Your density is multimodal**.  If your density has distinct
##' peaks, then most samplers will struggle to explore it well, and
##' even with a non-parallelised, non-vectorised sampler the parallel
##' tempering sampler will explore the space more efficiently.  In the
##' limit, a normal sampler may only explore a single peak in a model
##' with many such peaks and will never mix properly.
##'
##' @title Parallel Tempering Sampler
##'
##' @param n_rungs The number of **extra** chains to run, must be at
##'   least 1.  We will run a total of `n_rungs + 1` chains, with one
##'   of these being your target distribution and one being a direct
##'   sample from your base model (often your prior).
##'
##' @param beta A vector of beta values.  If provided, then `n_rungs`
##'   should not be provided, and `beta` should be a vector of at
##'   least length 2, where the first value is 1, the last value is 0
##'   and the values in between form a strictly decreasing sequence
##'   (i.e., no increases, no ties).
##'
##' @param sampler A sampler to use for the underlying chains.  You
##'   might use something like `monty_sampler_random_walk(vcv)` for a
##'   random walk sampler embedded in this parallel tempering scheme.
##'
##' @param base An optional base model, which must be provided if your
##'   model cannot be automatically decomposed into `prior +
##'   posterior` using [monty_model_split], or if you are not using
##'   this within a Bayesian context and you want to use an
##'   alternative easy-to-sample-from reference distribution.  We
##'   require that this model can be directly sampled from, that it
##'   accepts multiple parameters (as a matrix), that it is
##'   deterministic and we assume that it is cheap to compute
##'   (relative to the target).
##'
##' @return A `monty_sampler` object, which can be used with
##'   [monty_sample]
##'
##' @export
monty_sampler_parallel_tempering <- function(sampler, n_rungs = NULL,
                                             beta = NULL, base = NULL) {
  assert_is(sampler, "monty_sampler")
  beta <- validate_parallel_tempering_beta(n_rungs, beta)

  if (!is.null(base)) {
    assert_is(base, "monty_model")
    check_base_model(base, FALSE)
  }

  control <- list(n_rungs = length(beta) - 1L,
                  beta = beta,
                  sampler = sampler,
                  base = base)

  properties <- monty_sampler_properties(
    allow_multiple_parameters = FALSE,
    requires_allow_multiple_parameters = TRUE)

  monty_sampler(sprintf("Parallel Tempering [%s]", sampler$name),
                "monty_sampler_parallel_tempering",
                control,
                sampler_parallel_tempering_initialise,
                sampler_parallel_tempering_step,
                sampler_parallel_tempering_dump,
                sampler_parallel_tempering_combine,
                sampler_parallel_tempering_restore,
                sampler_parallel_tempering_details,
                properties = properties)
}


sampler_parallel_tempering_initialise <- function(state_chain, control, model,
                                                  rng) {
  ## TODO: For dust models this is going to require that they can
  ## respond appropriately to the number of different parameter sets,
  ## because on initialisation we'll call them with a single parameter
  ## set (as a vector) and then later we'll use a matrix of parameter
  ## sets.  This can be hidden away in the monty part I think, fairly
  ## easily.  After initialisation they will only require n_rungs
  ## samples every time.
  ##
  ## TODO: prevent observers
  n_rungs <- control$n_rungs

  model_base <- parallel_tempering_base(model, control)
  model_scaled <- parallel_tempering_scale(model, control)

  ## A caching calculator for the hot chain.  This will work in
  ## batches of size 'n_rungs' to match the main sampler.
  hot <- parallel_tempering_hot(model, control)

  pars <- cbind(state_chain$pars,
                direct_sample_many(n_rungs - 1, model_base, rng))
  sub_state_chain <- list(pars = pars, density = model_scaled$density(pars))

  ## And initialise the sampler:
  sub_state_sampler <- control$sampler$initialise(
    sub_state_chain,
    control$sampler$control,
    model_scaled,
    rng)

  env <- new.env(parent = emptyenv())
  env$model_scaled <- model_scaled
  env$model_base <- model_base
  env$sub_state_chain <- sub_state_chain
  env$sub_state_sampler <- sub_state_sampler
  env$hot <- hot
  env$even_step <- FALSE
  env$accept_swap <- integer(n_rungs)

  env
}


sampler_parallel_tempering_step <- function(state_chain, state_sampler,
                                            control, model, rng) {
  ## Unpack some useful quantities:
  beta <- control$beta
  n_rungs <- control$n_rungs
  idx_hot <- n_rungs + 1L
  sub_state_chain <- state_sampler$sub_state_chain

  ## At this point, if the chain state and the sub-sampler state
  ## differ we should do something about it.

  ## Update step for all chains except the hot chain:
  sub_state_chain <- control$sampler$step(
    sub_state_chain, state_sampler$sub_state_sampler,
    control$sampler$control, state_sampler$model_scaled, rng)

  ## And draw a sample from the hot chain (which might trigger a run)
  hot <- state_sampler$hot$sample(rng)

  ## Then we need to pool everything:
  pars <- cbind(sub_state_chain$pars, hot$pars)

  ## Compute the base density for everything; assume that this is
  ## quick enough that we can just recompute it each time.
  d_base <- state_sampler$model_base$density(pars)
  d_value <- c(sub_state_chain$density, d_base[[idx_hot]])
  d_target <- (d_value - (1 - beta) * d_base) / beta
  d_target[[idx_hot]] <- hot$density

  ## Start the communication step, moving the samples around across
  ## rungs:
  i1 <- seq(if (state_sampler$even_step) 2L else 1L, n_rungs, by = 2)
  i2 <- i1 + 1L

  ## Equation (6) in section 2.3 of https://doi.org/10.1111/rssb.12464
  ##
  ## NOTE: this is done against the *actual* densities, not the ones
  ## scaled by beta.  This can be wrapped in pmin(alpha, 0) but this
  ## has no practical effect.
  alpha <- (beta[i2] - beta[i1]) * (
    (d_target[i1] - d_base[i1]) - (d_target[i2] - d_base[i2]))
  u <- monty_random_n_real(length(i1), rng)
  accept <- log(u) < alpha

  if (any(accept)) {
    i_to <- c(i1[accept], i2[accept])
    i_from <- c(i2[accept], i1[accept])

    ## Shuffle around pars and the *real* densities 'base' and 'target'
    pars[, i_to] <- pars[, i_from]
    d_base[i_to] <- d_base[i_from]
    d_target[i_to] <- d_target[i_from]

    ## But recompute the density that we use in the sub sampler,
    ## because we've reordered the densities with respect to the
    ## vector of betas at this point.
    d_value[i_to] <-
      beta[i_to] * d_target[i_to] + (1 - beta[i_to]) * d_base[i_to]

    ## Then we save state for the sub chains, but we don't save the
    ## hot chain here at all.
    sub_state_chain$pars <- pars[, -idx_hot, drop = FALSE]
    sub_state_chain$density <- d_value[-idx_hot]
  }

  ## Update the real chain; this will have changed if we have either
  ## accepted a step from the sub sampler, or if we've swapped between
  ## chains.  We're avoiding update_state here because of this weird
  ## acceptance rule, but if we have observers, this is the right
  ## place to pull from in the case where i_to[[1]] is 1, but we need
  ## to observe from i_from[[1]]; we'll have these all in the sub
  ## chain state though, I think.
  state_chain$pars <- sub_state_chain$pars[, 1]
  state_chain$density <- sub_state_chain$density[[1]]

  state_sampler$sub_state_chain <- sub_state_chain
  state_sampler$even_step <- !state_sampler$even_step

  ## We track swap acceptances so that the beta values can be tuned,
  ## though this not yet implemented.
  state_sampler$accept_swap[i1] <- state_sampler$accept_swap[i1] + accept

  state_chain
}


sampler_parallel_tempering_dump <- function(state, control) {
  sub_state_sampler <- control$sampler$state$dump(state$sub_state_sampler,
                                                  control$sampler$control)
  list(accept_swap = state$accept_swap,
       even_step = state$even_step,
       hot = state$hot$dump(),
       sub_state_chain = state$sub_state_chain,
       sub_state_sampler = sub_state_sampler)
}


sampler_parallel_tempering_combine <- function(state, control) {
  join <- function(name, ...) {
    array_bind(arrays = lapply(state, "[[", name), ...)
  }
  accept_swap <- join("accept_swap", on = 2)
  even_step <- vlapply(state, "[[", "even_step")
  hot <- lapply(state, "[[", "hot")
  sub_state_chain <- lapply(state, "[[", "sub_state_chain")
  sub_state_sampler <- control$sampler$state$combine(
    lapply(state, "[[", "sub_state_sampler"))
  list(accept_swap = accept_swap,
       even_step = even_step,
       hot = hot,
       sub_state_chain = sub_state_chain,
       sub_state_sampler = sub_state_sampler)
}


sampler_parallel_tempering_restore <- function(chain_id, state_chain,
                                               state_sampler, control,
                                               model) {
  stopifnot(length(chain_id) == 1)

  model_base <- parallel_tempering_base(model, control)
  model_scaled <- parallel_tempering_scale(model, control)

  sub_state_chain <- state_sampler$sub_state_chain[[chain_id]]
  sub_state_sampler <- control$sampler$state$restore(
    chain_id,
    sub_state_chain,
    state_sampler$sub_state_sampler,
    control$sampler$control,
    model_scaled)
  hot <- parallel_tempering_hot(model, control, state_sampler$hot[[chain_id]])
  even_step <- state_sampler$even_step[[chain_id]]
  accept_swap <- state_sampler$accept_swap[, chain_id]

  env <- new.env(parent = emptyenv())
  env$model_scaled <- model_scaled
  env$model_base <- model_base
  env$sub_state_chain <- sub_state_chain
  env$sub_state_sampler <- sub_state_sampler
  env$hot <- hot
  env$even_step <- even_step
  env$accept_swap <- accept_swap
  env
}


sampler_parallel_tempering_details <- function(state, control) {
  list(accept_swap = state$accept_swap,
       sampler = control$sampler$state$details(state$sub_state_sampler))
}


## A caching direct sampler; we draw 'size' samples from the base
## distribution (perhaps the prior) and compute the true model density
## for all of these at once, then return these one at a time (so the
## actual calculation only happens every 'size' steps).  This means
## that if the model is configured to run in parallel with 'n_rungs'
## threads (which will be the same as 'size') then calculating from
## the prior will be as efficient as the samples from the other
## chains.
parallel_tempering_hot <- function(model, control, state = NULL) {
  model_base <- parallel_tempering_base(model, control)
  size <- control$n_rungs

  env <- new.env(parent = emptyenv())
  env$index <- size

  if (!is.null(state)) {
    list2env(state, env)
  }

  sample <- function(rng) {
    if (env$index >= size) {
      env$pars <- direct_sample_many(size, model_base, rng)
      env$density <- model$density(env$pars)
      env$index <- 0L
    }
    i <- env$index <- env$index + 1L
    list(pars = env$pars[, i, drop = FALSE],
         density = env$density[[i]])
  }

  dump <- function() {
    as.list(env, sorted = TRUE)
  }

  list(sample = sample, dump = dump)
}


## This will be used for everything *except* the hottest chain (beta =
## 0), and we set this up so that it can present itself as something
## that can be used in any of the normal samplers.
parallel_tempering_scale <- function(target, control) {
  base <- parallel_tempering_base(target, control)
  beta <- control$beta[-length(control$beta)]

  density <- function(x) {
    d_target <- target$density(x)
    d_base <- base$density(x)
    ## equivalently
    ##
    ## > beta * (d_target - d_base) + d_base
    ## >         ^^^^^^^^^^^^^^^^^
    ##           likelihood           ^^^^^^
    ##                                prior
    value <- beta * d_target + (1 - beta) * d_base
    ## We might have -Inf for the log density of the target model in
    ## the hottest chain (beta of 1) but finite log density for the
    ## base model, which means we do 0 * -Inf which is NaN, rather
    ## than -Inf. This check could be made more or less complicated;
    ## we could only consider the first and last chains (where
    ## multiplication by zero happens) and we could only consider
    ## cases where either component is -Inf.  This, however, works and
    ## is fast (fixes NaN and also NA_real_).
    value[is.na(value)] <- -Inf

    value
  }

  properties <- target$properties

  ## Gradient follows density above, will implement with HMC support
  gradient <- NULL
  properties$has_gradient <- FALSE

  ## Observer will require more work here and in the sampler to pull
  ## from correct chain
  ##
  ## I think that the observer here is actually the right way of doing
  ## this, but I don't think that it actually works well
  observer <- NULL
  properties$has_observer <- FALSE

  direct_sample <- base$direct_sample
  properties$has_direct_sample <- TRUE

  ## Combine the domains; this does nothing for the split model case
  ## but will save us some pain in the case where base is given
  ## explicitly.
  domain <- model_combine_domain(list(target, base), target$parameters)

  monty_model(
    list(parameters = target$parameters,
         density = density,
         gradient = gradient,
         observer = observer,
         domain = domain,
         direct_sample = direct_sample,
         ## Below here is error prone, but should be correct for now
         parameter_groups = target$parameter_groups,
         set_rng_state = target$rng_state$set,
         get_rng_state = target$rng_state$get),
    properties)
}


## Tidy this away somewhere, or better update the models so that this
## can easily be done without a wrapper.
direct_sample_many <- function(n, model, rng) {
  n_pars <- length(model$parameters)
  matrix(
    vapply(seq_len(n), function(i) model$direct_sample(rng), numeric(n_pars)),
    n_pars)
}


parallel_tempering_base <- function(model, control, call = parent.frame()) {
  if (is.null(control$base)) {
    base <- monty_model_split(model, prior_first = TRUE)[[1L]]
  } else {
    base <- control$base
  }
  check_base_model(base, split = is.null(control$base), call = call)
  if (!setequal(base$parameters, model$parameters)) {
    cli::cli_abort("'base' and 'model' must have the same parameters",
                   call = call)
  }
  monty_model_reorder(base, model$parameters)
}


validate_parallel_tempering_beta <- function(n_rungs, beta,
                                             call = parent.frame()) {
  if (is.null(n_rungs) && is.null(beta)) {
    cli::cli_abort("One of 'n_rungs' or 'beta' must be provided",
                   call = call)
  }
  if (!is.null(n_rungs) && !is.null(beta) && length(beta) != n_rungs + 1) {
    cli::cli_abort("Only one of 'n_rungs' or 'beta' may be provided",
                   call = call)
  }
  if (is.null(beta)) {
    if (n_rungs < 1) {
      cli::cli_abort("'n_rungs' must be at least 1",
                     call = call, arg = "n_rungs")
    }
    return(seq(1, 0, length.out = n_rungs + 1L))
  }
  if (length(beta) < 2) {
    cli::cli_abort("At least two 'beta' values are required")
  }
  if (beta[[1]] != 1) {
    cli::cli_abort("'beta[1]' must be 1", call = call, arg = "beta")
  }
  if (last(beta) != 0) {
    cli::cli_abort("'beta[{length(beta)}]' (the last value) must be 0",
                   call = call, arg = "beta")
  }
  if (!all(diff(beta) < 0)) {
    cli::cli_abort("'beta' must be strictly decreasing, with no ties",
                   call = call, arg = "beta")
  }
  beta
}


check_base_model <- function(base, split, call = parent.frame()) {
  if (split) {
    msg <- "Can't use base model split from 'model' as a base model"
  } else {
    msg <- "Can't use 'base' as a base model"
  }
  require_direct_sample(base, msg, call = call)
  require_multiple_parameters(base, msg, call = call)
  require_deterministic(base, msg, call = call)
}
