##' Create a "parallel tempering" sampler, which runs multiple chains
##' at once to try and improve mixing, or takes advantage of
##' vectorisation/parallelisation if your underlying model supports
##' it.  We have tested the implementation with the random walk
##' sampler ([monty_sampler_random_walk]) but this may work with other
##' samplers.
##'
##' We implement the sampler based on https://doi.org/10.1111/rssb.12464
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
##' @param sampler A sampler to use for the underlying chains.  You
##'   might use something like [monty_sampler_random_walk(vcv)] for a
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
monty_sampler_parallel_tempering <- function(n_rungs, sampler, base = NULL) {
  assert_scalar_size(n_rungs, allow_zero = FALSE)
  assert_is(sampler, "monty_sampler")

  if (!is.null(base)) {
    ## These checks also need doing on the split model, perhaps?
    assert_is(base, "monty_model")
    require_direct_sample(base, "Can't use 'base' as a base model")
    require_multiple_parameters(base, "Can't use 'base' as a base model")
    require_deterministic(base, "Can't use 'base' as a base model")
  }

  control <- list(n_rungs = n_rungs,
                  beta = seq(1, 0, length.out = n_rungs + 1),
                  sampler = sampler,
                  base = base,
                  idx_cold = 1L,
                  idx_hot = n_rungs + 1L)

  monty_sampler(sprintf("Parallel Tempering [%s]", sampler$name),
                "monty_sampler_parallel_tempering",
                control,
                sampler_parallel_tempering_initialise,
                sampler_parallel_tempering_step)
}


sampler_parallel_tempering_initialise <- function(state_chain, control, model,
                                                  rng) {
  ## TODO: prevent observers
  ## TODO: prevent stochastic models; there's some trick there with
  ## updating the state I think.
  if (length(dim2(state_chain$pars)) > 1) {
    ## This should be advertised by a property of the sampler, and we
    ## should also be able to support this fairly easily.
    cli::cli_abort("Can't use parallel tempering with multiple parameter sets")
  }

  if (is.null(control$base)) {
    base <- monty_model_split(model, prior_first = TRUE)[[1L]]
  } else {
    ## TODO: we could allow setequal and reorder
    if (!identical(base$parameters, model$parameters)) {
      cli::cli_abort("'base' and 'model' must have the same parameters")
    }
  }

  n_rungs <- control$n_rungs
  beta <- control$beta
  stopifnot(last(beta) == 0)

  ## TODO: For dust models this is going to require that they can
  ## respond appropriately to the number of different parameter sets,
  ## because on initialisation we'll call them with a single parameter
  ## set (as a vector) and then later we'll use a matrix of parameter
  ## sets.  This can be hidden away in the monty part I think, fairly
  ## easily.  After initialisation they will only require n_rungs
  ## samples every time.

  ## A caching calculator for the hot chain.  This will work in
  ## batches of size 'n_rungs' to match the main sampler.
  hot <- parallel_tempering_hot(model, base, n_rungs)

  model_scaled <- parallel_tempering_scale(model, base, beta[-length(beta)])

  pars <- cbind(state_chain$pars, direct_sample_many(n_rungs - 1, base, rng))
  ## This bit of initialisation could be tidied up I think; in
  ## particular, who is responsible for tracking the additional state
  ## associated with the auxillary chains.  For now, leave them in the
  ## chain state but that is not ideal.
  density <- model_scaled$density(pars)
  sub_state_chain <- model_scaled$model$last_density()

  stopifnot(density[[1]] == state_chain$density,
            sub_state_chain$density[[1]] == state_chain$density)

  ## And initialise the sampler:
  sub_state_sampler <- control$sampler$initialise(
    sub_state_chain,
    control$sampler$control,
    model_scaled,
    rng)

  ## We also need this for the base model:
  hot <- parallel_tempering_hot(model, base, n_rungs)

  env <- new.env(parent = emptyenv())
  env$model <- model_scaled
  env$base <- base
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
  sub_state_chain <- state_sampler$sub_state_chain

  ## Update step for all chains except the hot chain:
  sub_state_chain <- control$sampler$step(
    sub_state_chain, state_sampler$sub_state_sampler,
    control$sampler$control, state_sampler$model, rng)

  ## Then we update the sub components of density for these where we
  ## accepted a new point:
  density <- state_sampler$model$model$last_density()
  accepted <- apply(density$pars == sub_state_chain$pars, 2, all)
  if (any(accepted)) {
    sub_state_chain$details[, accepted] <-
      density$details[, accepted]
  }

  ## And draw a sample from the hot chain (which might trigger a run)
  hot <- state_sampler$hot$sample(rng)
  idx_hot <- n_rungs + 1L

  ## Then we need to pool everything to make the bookkeeping here a
  ## bit more palatable:
  pars <- cbind(sub_state_chain$pars, hot$pars)

  ## Compute the base density for everything; assume that this is
  ## quick enough that we can just recompute it each time.
  d_base <- state_sampler$base$density(pars)
  d_value <- c(sub_state_chain$details["value", ], d_base[[idx_hot]])
  d_target <- (d_value - (1 - beta) * d_base) / beta
  d_target[[idx_hot]] <- hot$density

  details <- rbind(target = d_target, base = d_base, value = d_value)

  ## TODO: we can really simplify everything if we tolerate just
  ## computing 'base' again really, and then treating the hot chain a
  ## bit separately, because we can do all the calculations without
  ## having to make the model stateful.

  ## At this point we can easily confirm that the densities have the
  ## expected relationship:
  ## details["value", ] -
  ##   (beta * details["target", ] + (1 - beta) * details["base", ])
  ## details["value", ] -
  ##   (beta * (details["target", ] - details["base", ]) + details["base", ])

  ## Start the communication step, moving the samples around across
  ## rungs:
  i1 <- seq(if (state_sampler$even_step) 2L else 1L, n_rungs, by = 2)
  i2 <- i1 + 1L

  ## Equation (6) in section 2.3 of https://doi.org/10.1111/rssb.12464
  ##
  ## NOTE: this is done against the *actual* densities, not the ones
  ## scaled by beta.
  ##
  ## TODO: I think that the pmin here can be dropped, because it takes
  ## a value greater than zero and reduces it to zero, which is still
  ## higher than any log(U(0, 1)), so does not affect the acceptance.
  ## We can hash samples later an compare.
  target <- details["target", ]
  base <- details["base", ]
  alpha <- pmin(
    0,
    (beta[i2] - beta[i1]) * ((target[i1] - base[i1]) - (target[i2] - base[i2])))
  u <- monty_random_n_real(length(i1), rng)
  accept <- log(u) < alpha

  if (any(accept)) {
    i_to <- c(i1[accept], i2[accept])
    i_from <- c(i2[accept], i1[accept])

    pars[, i_to] <- pars[, i_from]
    details[, i_to] <- details[, i_from]

    ## The value of 'value' here will be wrong, so update that for the
    ## current 'beta'.  This can also be updated based on some sort of
    ## ratio of betas but the actual calculation here is easy enough
    ## compared with everything else that we're doing.  We can do this
    ## entirely with the 'i_to' index, as that captures all the
    ## destination indices.
    details["value", i_to] <- beta[i_to] * details["target", i_to] +
      (1 - beta[i_to]) * details["base", i_to]

    ## Then we save state for the sub chains
    idx_hot <- n_rungs + 1L
    sub_state_chain$pars <- pars[, -idx_hot, drop = FALSE]
    sub_state_chain$details <- details[, -idx_hot, drop = FALSE]
    sub_state_chain$density <- details["value", -idx_hot]

    ## Update the real chain; we're avoiding update_state here though.
    ## If we have observers, this is the right place to pull from in
    ## the case where i_to[[1]] is 1, but we need to observe from
    ## i_from[[1]]; we'll have these all in the sub chain state
    ## though, I think
    state_chain$pars <- sub_state_chain$pars[, 1]
    state_chain$density <- sub_state_chain$density[[1]]
  }

  state_sampler$sub_state_chain <- sub_state_chain
  state_sampler$accept_swap[i1] <- state_sampler$accept_swap[i1] + accept
  state_sampler$even_step <- !state_sampler$even_step

  state_chain
}


parallel_tempering_hot <- function(model, base, size) {
  force(model)
  force(base)

  env <- new.env(parent = emptyenv())
  env$size <- size
  env$index <- size

  sample <- function(rng) {
    if (env$index >= size) {
      env$pars <- direct_sample_many(size, model, rng)
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

  restore <- function(state) {
    list2env(state, env)
  }

  list(sample = sample, dump = dump, restore = restore)
}


## This will be used for everything *except* the hottest chain (beta =
## 0), and we set this up so that we can adva
parallel_tempering_scale <- function(target, base, beta) {
  env <- new.env()
  env$beta <- beta

  density <- function(x, beta = env$beta) {
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

    ## After accepting or rejecting points, we also need to keep track
    ## of their consitutent bits.  This is relatively easy to do if we
    ## are allowed to recompute 'base' and if beta is not zero (in
    ## which case, the target disappears) but instead here we save all
    ## the components of the last density calculation and later on
    ## we'll work out which of these made it through.  We might change
    ## this approach later though.
    env$last <- list(pars = x,
                     density = value,
                     details = rbind(target = d_target,
                                     base = d_base,
                                     value = value))

    value
  }

  last_density <- function() {
    env$last
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
         get_rng_state = target$rng_state$get,
         ## Extra for PT:
         last_density = last_density),
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
