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
monty_sampler_nested_random_walk <- function(vcv, boundaries = "reflect",
                                      rerun_every = Inf, rerun_random = TRUE) {
  if (!is.list(vcv)) {
    cli::cli_abort(
      "Expected a list for 'vcv'",
      arg = "vcv")
  }
  
  if (!setequal(names(vcv), c("base", "groups"))) {
    cli::cli_abort("Expected 'vcv' to have elements 'base' and 'groups'",
                   arg = "vcv")
  }
  if (!is.null(vcv$base)) {
    check_vcv(vcv$base, allow_3d = TRUE, call = environment())
  }
  if (!is.list(vcv$groups)) {
    cli::cli_abort("Expected 'vcv$groups' to be a list")
  }
  if (length(vcv$groups) < 1) {
    cli::cli_abort("Expected 'vcv$groups' to have at least one element")
  }
  for (i in seq_along(vcv$groups)) {
    check_vcv(vcv$groups[[i]], allow_3d = TRUE,
              name = sprintf("vcv$groups[%d]", i), call = environment())
  }
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

  monty_sampler("Random walk",
                "monty_nested_random_walk",
                control,
                sampler_nested_random_walk_initialise,
                sampler_nested_random_walk_step,
                sampler_nested_random_walk_dump,
                sampler_nested_random_walk_combine,
                sampler_nested_random_walk_restore)
}


## The core sampler functions: initalise, step, dump, restore
sampler_nested_random_walk_initialise <- function(state_chain, control, model, rng) {
  pars <- state_chain$pars

  ## TODO: Samplers need to cope with this, and we could have them
  ## advertise this I think, as part of their properties, once we have
  ## that sorted out.
  multiple_parameters <- length(dim2(pars)) > 1
  if (multiple_parameters) {
    ## this is properly enforced elsewhere, but we assert here to be
    ## safe.
    stopifnot(model$properties$allow_multiple_parameters)
  }
  
  
  
  ## We don't actually need to return an environment here, because all
  ## our things that are modified by reference (rerun) will already be
  ## captured in an environment
  list(proposal = make_nested_random_walk_proposal(control, model, pars),
       prior = make_nested_random_walk_prior(model),
       rerun = make_rerun(control, model))
}


sampler_nested_random_walk_step <- function(state_chain, state_sampler, control,
                                            model, rng) {
  if (control$rerun) {
    rerun <- state_sampler$rerun(rng)
    if (any(rerun)) {
      ## This is currently just setup assuming we are not using multiple
      ## parameters as currently they cannot be used with stochastic models,
      ## while the rerun is only used with stochastic models
      state_chain$density <- model$density(state_chain$pars, by_group = TRUE)
      state_chain$density_by_group <- attr(state_chain$density, "by_group")
    }
  }
  
  if (!is.null(state_sampler$proposal$base)) {
    pars_next <- state_sampler$proposal$base(state_chain$pars, rng)
    
    reject_some <- boundaries == "reject" &&
      !all(i <- is_parameters_in_domain(pars_next, model$domain))
    if (reject_some) {
      density_next <- rep(-Inf, length(state_chain$density))
      density_by_group_next <- array(-Inf, dim2(state_chain$density_by_group))
      if (any(i)) {
        if (!is.null(state_sampler$prior)) {
          density_next[i] <- state_chain$density[i] - 
            state_sampler$prior$density(state_chain$pars[, i, drop = FALSE]) +
            state_sampler$prior$density(pars_next[, i, drop = FALSE])
          density_by_group_next[, i] <- state_chain$density_by_group[, i]
        } else {
          density_next_i <- model$density(pars_next[, i, drop = FALSE],
                                          by_group = TRUE)
          density_next[i] <- density_next_i
          density_by_group_next[, i] <- attr(density_next_i, "by_group")
        }
      }
    } else {
      if (!is.null(state_sampler$prior)) {
        density_next <- state_chain$density - 
          state_sampler$prior$density(state_chain$pars) +
          state_sampler$prior$density(pars_next)
        density_by_group_next <- state_chain$density_by_group
      } else {
        density_next <- model$density(pars_next, by_group = TRUE)
        density_by_group_next <- attr(density_next, "by_group")
      }
      
    }
    
    density_next <- structure(density_next, "by_group" = density_by_group_next)
    
    accept <- density_next - state$density > log(monty_random_real(rng))
    if (any(accept)) {
      if (!all(accept)) {
        ## Retain some older parameters
        i <- which(!accept)
        pars_next[, i] <- state$pars[, i]
        density_next <- model$density(pars_next, by_group = TRUE)
        density_by_group_next <- attr(density_next, "by_group")
      }
      state$pars <- pars_next
      state$density <- density_next
      internal$density_by_group <- density_by_group_next
      ## TODO: mrc-5862
      if (model$properties$has_observer) {
        state$observation <- model$observer$observe()
      }
    }
  }
  
  pars_next <- internal$proposal$groups(state$pars, rng)
  
  reject_some <- boundaries == "reject" &&
    !all(i <- is_parameters_in_domain_groups(pars_next, model$domain,
                                             model$parameter_groups))
  
  ## This bit is potentially inefficient - for any proposed parameters out of
  ## bounds I substitute in the current parameters, so that we can run the
  ## density on all groups. Ideally we would want to only run the density on
  ## groups with all parameters in bounds. A bit fiddly to do that in a nice
  ## way when doing simultaneous sampling
  if (reject_some) {
    density_next <- rep(-Inf, length(state$density))
    density_by_group_next <- array(-Inf, dim2(internal$density_by_group))
    if (any(i)) {
      if (internal$multiple_parameters) {
        for (j in seq_len(ncol(i))) {
          if (!all(i[, j])) {
            i_group <- model$parameter_groups %in% which(!i[, j])
            pars_next[i_group, j] <- state$pars[i_group, j]
          }
        }
      } else {
        i_group <- model$parameter_groups %in% which(!i)
        pars_next[i_group] <- state$pars[i_group]
      }
      density_next <- model$density(pars_next, by_group = TRUE)
      density_by_group_next <- attr(density_next, "by_group")
    }
  } else {
    density_next <- model$density(pars_next, by_group = TRUE)
    density_by_group_next <- attr(density_next, "by_group")
  }
  
  accept <- density_by_group_next - internal$density_by_group >
    log(monty_random_n_real(dim2(density_by_group_next)[1], rng))
  
  if (any(accept)) {
    if (!all(accept)) {
      ## Retain some older parameters
      if (internal$multiple_parameters) {
        for (j in seq_len(ncol(accept))) {
          if (!all(accept[, j])) {
            i <- model$parameter_groups %in% which(!accept[, j])
            pars_next[i, j] <- state$pars[i, j]
          }
        }
      } else {
        i <- model$parameter_groups %in% which(!accept)
        pars_next[i] <- state$pars[i]
      }
      ## If e.g. density is provided by a particle filter, would this bit
      ## mean rerunning it? Increases time cost if so, and would result in
      ## new value of density (different to that which was accepted)
      density_next <- model$density(pars_next, by_group = TRUE)
      density_by_group_next <- attr(density_next, "by_group")
    }
    state$pars <- pars_next
    state$density <- c(density_next)
    internal$density_by_group <- density_by_group_next
    ## TODO: mrc-5862
    if (model$properties$has_observer) {
      state$observation <- model$observer$observe()
    }
  }
  state

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

  ## TODO: rng is not needed through here and can be removed
  update_state(state_chain, pars_next, density_next, accept, model, rng)
}


sampler_nested_random_walk_dump <- function(state) {
  if (is.null(state$rerun)) {
    return(NULL)
  }
  list(rerun_state = attr(state$rerun, "data"))$step
}


sampler_nested_random_walk_combine <- function(state) {
  if (all(vlapply(state, is.null))) {
    return(NULL)
  }
  state[[1]]
}


sampler_nested_random_walk_restore <- function(chain_id, state_chain,
                                        state_sampler, control,
                                        model) {
  pars <- state_chain$pars
  list(proposal = make_nested_random_walk_proposal(control, model, pars),
       rerun = make_rerun(control, model, state_sampler$rerun_state))
}


make_nested_random_walk_proposal <- function(control, model, pars) {
  vcv <- sampler_validate_nested_vcv(control$vcv, model$parameter_groups, pars)
  make_nested_random_walk_proposal_fn(vcv, model$domain, control$boundaries)
}


make_nested_random_walk_prior <- function(model) {
  
  if (!inherits(model$model$data, "_combined_model")) {
    return(NULL)
  }
  
  model_split <- monty_model_split(model, prior_first = TRUE)
  has_base <- lapply(model_split, 
                     function (m) sum(m$parameter_groups == 0) != 0)
  if (has_base[[1]] && !has_base[[2]]) {
    return(model_split[[1]])
  } else {
    return(NULL)
  }
  
}

sampler_validate_nested_vcv <- function(vcv, parameter_groups, pars,
                                        call = NULL) {
  i_base <- parameter_groups == 0
  n_base <- sum(i_base)
  has_base <- n_base != 0
  n_groups <- max(parameter_groups)
  i_group <- lapply(seq_len(n_groups), function(i) which(parameter_groups == i))
  
  if (NROW(vcv$base) != n_base) {
    cli::cli_abort(
      c("Incompatible number of base parameters in your model and sampler",
        i = paste("Your model has {n_base} base parameters, but 'vcv$base'",
                  "implies {NROW(vcv$base)} parameters")),
      call = call)
  }
  if (length(vcv$groups) != n_groups) {
    cli::cli_abort(
      c("Incompatible number of parameter groups in your model and sampler",
        i = paste("Your model has {n_groups} parameter groups, but",
                  "'vcv$groups' has {length(vcv$groups)} groups")),
      call = call)
  }
  n_pars_by_group <- lengths(i_group)
  n_pars_by_group_vcv <- vnapply(vcv$groups, nrow)
  err <- n_pars_by_group_vcv != n_pars_by_group
  if (any(err)) {
    detail <- sprintf(
      "Group %d has %d parameters but 'vcv$groups[[%d]]' has %d",
      which(err), n_pars_by_group[err],
      which(err), n_pars_by_group_vcv[err])
    cli::cli_abort(
      c("Incompatible number of parameters within parameter group",
        set_names(detail, "i")),
      call = call)
  }
  
  if (has_base) {
    if (is.matrix(pars)) {
      vcv$base <- sampler_validate_vcv(vcv$base, pars[i_base, , drop = FALSE])
    } else {
      vcv$base <- sampler_validate_vcv(vcv$base, pars[i_base])
    }
  }
  
  for (i in seq_len(n_groups)) {
    if (is.matrix(pars)) {
      vcv$groups[[i]] <-
        sampler_validate_vcv(vcv$groups[[i]],
                             pars[i_group[[i]], , drop = FALSE])
    } else {
      vcv$groups[[i]] <-
        sampler_validate_vcv(vcv$groups[[i]], pars[i_group[[i]]])
    }
  }
  
  vcv
}


make_nested_random_walk_proposal_fn <- function(vcv, parameter_groups,
                                                pars, domain, 
                                                boundaries = "reflect") {
  i_base <- parameter_groups == 0
  n_base <- sum(i_base)
  has_base <- n_base != 0
  n_groups <- max(parameter_groups)
  i_group <- lapply(seq_len(n_groups), function(i) which(parameter_groups == i))
  
  if (has_base) {
    if (is.matrix(pars)) {
      mvn_base <- make_random_walk_proposal_fn(
        vcv$base, domain[i_base, , drop = FALSE], boundaries)
      proposal_base <- function(x, rng) {
        ## This approach is likely to be a bit fragile, so we'll
        ## probably want some naming related verification here soon too.
        x[i_base] <- mvn_base(x[i_base, ], rng)
        x
      }
    } else {
      mvn_base <- make_random_walk_proposal_fn(
        vcv$base, domain[i_base, , drop = FALSE], boundaries)
      proposal_base <- function(x, rng) {
        ## This approach is likely to be a bit fragile, so we'll
        ## probably want some naming related verification here soon too.
        x[i_base] <- mvn_base(x[i_base], rng)
        x
      }
    }
    
  } else {
    proposal_base <- NULL
  }
  
  mvn_groups <- lapply(seq_len(n_groups), function(i) {
    make_random_walk_proposal_fn(vcv$groups[[i]],
                                 domain[i_group[[i]], , drop = FALSE],
                                 boundaries)
  })
  proposal_groups <- function(x, rng) {
    for (i in seq_len(n_groups)) {
      if (is.matrix(x)) {
        x[i_group[[i]], ] <- mvn_groups[[i]](x[i_group[[i]], ], rng)
      } else {
        x[i_group[[i]]] <- mvn_groups[[i]](x[i_group[[i]]], rng)
      }
    }
    x
  }
  
  list(base = proposal_base,
       groups = proposal_groups)
}
