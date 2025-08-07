initialise_state <- function(pars, model, rng) {
  initialise_rng_state(model, rng)
  density <- model$density(pars)
  if (model$properties$has_observer) {
    observation <- model$observer$observe()
  } else {
    observation <- NULL
  }
  list(pars = pars, density = density, observation = observation)
}


update_state <- function(state, pars, density, accept, model, rng) {
  if (any(accept)) {
    if (is.matrix(state$pars)) {
      state$pars[, accept] <- pars[, accept]
      state$density[accept] <- density[accept]
      state
    } else {
      ## Some sampler/model combinations might produce a 1-column
      ## matrix for the updated pars, but we should store this as a
      ## vector regardless:
      state$pars <- drop(pars)
      state$density <- density
      if (model$properties$has_observer) {
        state$observation <- model$observer$observe()
      }
    }
  }
  state
}


initialise_rng_state <- function(model, rng) {
  if (isTRUE(model$properties$is_stochastic)) {
    state <- monty_rng_state(rng)
    model$rng_state$set(monty_rng_jump(state))
  }
}


sampler_validate_vcv <- function(vcv, pars, call = NULL) {
  if (is.matrix(pars)) {
    n_sets <- ncol(pars)
    n_pars <- nrow(pars)
  } else {
    n_sets <- 1
    n_pars <- length(pars)
  }

  n_pars_vcv <- nrow(vcv)
  if (n_pars != n_pars_vcv) {
    cli::cli_abort(
      "Incompatible length parameters ({n_pars}) and vcv ({n_pars_vcv})",
      call = call)
  }

  if (length(dim(vcv)) == 3) {
    n_sets_vcv <- dim(vcv)[[3]]
    if (n_sets == 1 && n_sets_vcv == 1) {
      dim(vcv) <- dim(vcv)[1:2]
    } else if (n_sets_vcv == 1) {
      vcv <- array(vcv, c(dim(vcv)[1:2], n_sets))
    } else if (n_sets_vcv != n_sets) {
      cli::cli_abort(
        c(paste("Incompatible number of parameter sets ({n_sets}) and slices",
                "in vcv ({n_sets_vcv})"),
          i = paste("You configured the sampler to expect {n_sets_vcv}",
                    "parameter set{?s}, but {n_sets} parameter set{?s}",
                    "were provided when the sampler was initialised")),
        call = call)
    }
  } else if (n_sets > 1) {
    vcv <- array(vcv, c(dim(vcv)[1:2], n_sets))
  }

  vcv
}


## This is used in both the normal and adaptive random walk sampler
make_random_walk_proposal_fn <- function(vcv, domain, boundaries) {
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


make_rerun <- function(control, model, initial = NULL) {
  no_rerun <- !control$rerun ||
    !is.finite(control$rerun_every) ||
    !model$properties$is_stochastic
  if (no_rerun) {
    return(NULL)
  }

  every <- control$rerun_every

  if (control$rerun_random) {
    ret <- function(rng) monty_random_real(rng) < 1 / every
  } else {
    data <- new.env(parent = emptyenv())
    data$i <- initial %||% 0L
    ret <- function(rng) {
      data$i <- data$i + 1L
      rep(data$i %% every == 0, length(rng))
    }
    attr(ret, "data") <- data
  }
  ret
}


check_parameter_groups <- function(x, n_pars, name = deparse(substitute(x)),
                                   call = NULL) {
  if (!rlang::is_integerish(x)) {
    cli::cli_abort("Expected '{name}' to be integer-like", call = call)
  }
  if (length(x) != n_pars) {
    cli::cli_abort(
      paste("Expected '{name}' to have length {n_pars}, but it had length",
            "{length(x)}"),
      call = call)
  }
  if (min(x) < 0) {
    cli::cli_abort("Invalid negative group in '{name}'", call = call)
  }
  n_groups <- max(x)
  msg <- setdiff(seq_len(n_groups), x)
  if (length(msg) > 0) {
    cli::cli_abort(
      c("Missing groups from '{name}'",
        i = paste("I expected all integers from 1 to {n_groups} to be present",
                  "in your parameter groups vector, but you are missing",
                  "{msg}")),
      call = call)
  }
}
