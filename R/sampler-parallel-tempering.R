##' Create a "parallel tempering" sampler, which runs multiple chains
##' at once to try and improve mixing, or takes advantage of
##' vectorisation/parallelisation if your underlying model supports
##' it.  Currently uses a random walk sampler
##' [monty_sampler_random_walk] as the underlying sampler, but we will
##' make this configurable in a future version.
##'
##' We implement the sampler based on https://arxiv.org/pdf/1905.02939
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
##' this case, you'll stil be using much more CPU time but will be
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
##'   least 1 and probably should be an even number.
##'
##' @param vcv The variance covariance matrix for the random walk
##'   sampler (this interface will change)
##'
##' @param base An optional base model, which must be provided if your
##'   model cannot be automatically decomposed into `prior +
##'   posterior` using [monty_model_split], or if you are not using
##'   this within a Bayesian context and you want to use an
##'   alternative easy-to-sample-from reference distribution.
##'
##' @return A `monty_sampler` object, which can be used with
##'   [monty_sample]
##'
##' @export
monty_parallel_tempering <- function(n_rungs, vcv, base = NULL) {
  ## We may want to warn if users provide an odd number of rungs as we
  ## do a different number of swaps on even/odd steps.  Marc to check
  ## if this would affect efficiency of the sampler.
  assert_scalar_size(n_rungs, allow_zero = FALSE)

  ## Use a fixed schedule for now, later we'll need to allow this to
  ## be specified so that a schedule learned from the swapping rates
  ## can be used.
  ##
  ## beta = 1 refers to the target model, while beta = 0 the base
  ## model ("reference" model in the paper)
  beta <- seq(1, 0, length.out = n_rungs + 1)

  ## The indexes of swaps at each step odd and even steps:
  swap <- lapply(1:0, function(i) which(seq_len(n_rungs) %% 2 == i))

  internal <- new.env()
  internal$even_step <- FALSE
  internal$accept_swap <- integer(n_rungs)

  ## TODO: we might need to accept an initialised sampler here,
  ## otherwise dots.  Doing that means that control over the us being
  ## a multichain sampler is tricky and we might need to break apart
  ## and rebuild the sampler (so samplers might need to "know" how
  ## many chains they are sampling at the same time, which is fine and
  ## might help with some validation and definitely with the issue
  ## around proposals on the hot chain). [mrc-5982]
  ##
  ## TODO: we need to tweak this so that the last one does not
  ## actually use the sampler but instead does a direct sample.
  ## This is going to require some changes to the sampler code above
  ## because we simply don't take the step, but ideally we will do
  ## the calculation at the same time.
  sampler <- monty_sampler_random_walk(vcv)

  if (!is.null(base)) {
    assert_is(base, "monty_model")
    require_direct_sample(base, "Can't use 'base' as a base model")
    require_multiple_parameters(base, "Can't use 'base' as a base model")
  }

  initialise <- function(pars, model, rng) {
    if (is.null(base)) {
      internal$base <- monty_model_split(model, prior_first = TRUE)[[1L]]
    } else {
      if (!identical(base$parameters, model$parameters)) {
        cli::cli_abort("'base' and 'model' must have the same parameters")
      }
      internal$base <- base
    }

    n_pars <- length(model$parameters)

    ## First, augment our parameters with ones sampled from the base
    ## model for all the extra chains.
    ##
    ## TODO: interface for doing this sampling added to all models
    ## (mrc-5292)
    pars_extra <- matrix(vapply(seq_len(n_rungs),
                                function(i) internal$base$direct_sample(rng),
                                numeric(n_pars)),
                         n_pars)
    pars_full <- cbind(pars, pars_extra, deparse.level = 0)

    ## Build a new model which is the scaled multi-parameter version
    ## of our target model, with the hottest chain being 'base'.  From
    ## this point we'll ignore the model passed in, which is quite
    ## weird but we'll fix that later.
    internal$model <- parallel_tempering_scale(model, internal$base, beta)

    ## Use this new model to initialise our multi-parameter sample
    state <- sampler$initialise(pars_full, internal$model, rng)
    internal$state <- state

    list(pars = state$pars[, 1], density = state$density[[1]])
  }

  ## I don't like how we're basically just ignoring 'model' and
  ## 'state' here, and suggests some changes needed for the next
  ## iteration of samplers, for now it's ok.
  ##
  ## In the next version, as we head to something generally useful, we
  ## might define a base R6 class that everything can inherit from?
  ##
  ## Alternatively, initialise could pass back internal state and we
  ## could recieve it here; that might be nicer, actually.
  step <- function(state, model, rng) {
    ## Make sure we're in sync.  This could be an assignment or an
    ## assertion.
    stopifnot(
      all(internal$state$pars[, 1] == state$pars),
      all(internal$state$density[1] == state$density))

    ## Update step, later this will propose/accept hot chain differently
    state <- sampler$step(internal$state, internal$model, rng)

    ## We can get the uncorrected densities from before here:
    density <- internal$model$model$last_density()

    ## communication step
    i1 <- swap[[internal$even_step + 1]]
    i2 <- i1 + 1L

    ## Equation (6) in section 2.3 of https://arxiv.org/pdf/1905.02939
    ##
    ## TODO: Marc to check and perhaps harmonise names
    ## TODO: Marc: I have reversed the i's on beta here, seems required?
    ##
    ## NOTE: this is done against the *actual* densities, not the ones
    ## scaled by beta.
    alpha <- pmin(
      0,
      (beta[i1] - beta[i2]) * (density$target[i2] - density$target[i1]))
    u <- rng$random_real(length(i1))
    accept <- log(u) < alpha

    if (any(accept)) {
      i_to <- c(i1[accept], i2[accept])
      i_from <- c(i2[accept], i1[accept])
      state$pars[, i_to] <- state$pars[, i_from]
      d_target <- density$target[i_from]
      d_base <- density$base[i_from]
      state$density[i_to] <- beta[i_to] * d_target * (1 - beta[i_to]) * d_base
    }
    internal$accept_swap[i1] <- internal$accept_swap[i1] + accept

    ## For observations here we'll need to pass the swap back into
    ## 'internal$model', so that it knows if it was index 1 or 2 that
    ## was the last cold chain and dirct the call to 'observe()' to
    ## that submodel.
    internal$even_step <- !internal$even_step
    internal$state <- state

    list(pars = state$pars[, 1], density = state$density[[1]])
  }

  finalise <- function(state, model, rng) {
    list(accept_swap = internal$accept_swap)
  }

  get_internal_state <- function() {
    list(accept_swap = internal$accept_swap)
  }

  set_internal_state <- function(state) {
    internal$accept_swap <- state$accept_swap
  }

  monty_sampler("Parallel Tempering",
                "monty_parallel_tempering",
                initialise,
                step,
                finalise,
                get_internal_state,
                set_internal_state)
}


parallel_tempering_scale <- function(target, base, beta) {
  env <- new.env()

  density <- function(x) {
    d_target <- target$density(x)
    d_base <- base$density(x)
    env$density <- list(target = d_target, base = d_base)
    ## equivalently
    ##
    ## > beta * (d_target - d_base) * d_base
    ## >         ^^^^^^^^^^^^^^^^^
    ##           likelihood           ^^^^^^
    ##                                prior
    beta * d_target + (1 - beta) * d_base
  }

  if (target$properties$has_gradient) {
    gradient <- function(x) {
      g_target <- target$gradient(x)
      g_base <- base$gradient(x)
      env$gradient <- list(target = g_target, base = g_base)
      ## calculation as above for density
      beta * g_target + (1 - beta) * g_base
    }
  } else {
    gradient <- NULL
  }

  if (target$properties$has_observer) {
    cli::cli_abort("Observers in parallel tempering not yet supported")
  } else {
    observer <- NULL
  }

  monty_model(
    list(parameters = target$parameters,
         density = density,
         gradient = gradient,
         observer = observer,
         ## Below here is error prone, but should be correct for now
         parameter_groups = target$parameter_groups,
         domain = target$domain, # or base? or combine?
         direct_sample = target$direct_sample,
         set_rng_state = target$rng_state$set,
         get_rng_state = target$rng_state$get,
         ## Extra for PT:
         last_density = function() env$density,
         last_gradient = function() env$gradient),
    target$properties)
}
