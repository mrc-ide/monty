sir_filter_monty <- function(data, n_particles, deterministic = FALSE,
                             save_trajectories = FALSE, seed = NULL) {
  parameters <- c("beta", "gamma")
  env <- new.env()
  base <- list(N = 1000, I0 = 10, beta = 0.2, gamma = 0.1, exp_noise = 1e6)

  get_rng_state <- function() {
    c(monty_rng_state(env$rng$filter),
      monty_rng_state(env$rng$system))
  }

  set_rng_state <- function(rng_state) {
    n_streams <- n_particles + 1
    r <- matrix(
      monty_rng$new(n_streams = n_streams, seed = rng_state)$state(),
      ncol = n_streams)
    env$rng <- list(
      filter = monty_rng_create(1, r[, 1], deterministic),
      system = monty_rng_create(n_particles, c(r[, -1]), deterministic))
  }

  set_rng_state(seed)

  density <- function(x) {
    pars <- base
    pars[parameters] <- x
    res <- sir_filter(pars, data, n_particles, env$rng, save_trajectories)
    if (save_trajectories) {
      env$trajectories <- res$trajectories
    }
    res$log_likelihood
  }

  if (save_trajectories) {
    observer <- monty_observer(
      function() {
        i <- 1L
        trajectories <- env$trajectories[c(2, 4), i, , drop = FALSE]
        dim(trajectories) <- dim(trajectories)[-2]
        list(trajectories = trajectories)
      })
  } else {
    observer <- NULL
  }

  monty_model(
    list(density = density,
         parameters = parameters,
         observer = observer,
         set_rng_state = set_rng_state,
         get_rng_state = get_rng_state),
    monty_model_properties(is_stochastic = !deterministic))
}


sir_filter <- function(pars, data, n_particles, rng,
                       save_trajectories = FALSE) {
  y <- list(S = pars$N - pars$I0, I = pars$I0, R = 0, cases = 0)
  packer <- monty_packer(names(y))
  state <- matrix(packer$pack(y), 4, n_particles)
  time <- 0
  dt <- 1
  ll <- 0
  exp_noise <- rep_len(pars$exp_noise, n_particles)
  i_cases <- packer$index()$cases
  if (save_trajectories) {
    trajectories <- array(NA_real_, c(length(y), n_particles, nrow(data)))
  } else {
    trajectories <- NULL
  }

  for (i in seq_len(nrow(data))) {
    from <- time
    to <- data$time[[i]]
    state <- sir_run(from, to, dt, state, packer, pars, rng$system)
    noise <- monty_random_exponential_rate(exp_noise, rng$system)
    lambda <- state[i_cases, ] + noise
    tmp <- dpois(data$incidence[[i]], lambda, log = TRUE)
    w <- exp(tmp - max(tmp))
    ll <- ll + log(mean(w)) + max(tmp)
    u <- monty_random_real(rng$filter)
    k <- dust_resample_weight(w, u)

    if (save_trajectories) {
      trajectories[, , i] <- state
      trajectories <- trajectories[, k, , drop = FALSE] # slow but correct...
    }

    state <- state[, k, drop = FALSE]
    time <- to
  }

  list(log_likelihood = ll, trajectories = trajectories)
}


sir_run <- function(from, to, dt, state, packer, pars, rng) {
  state <- packer$unpack(state)
  for (time in seq(from, to, by = dt)[-1]) {
    state <- sir_step(time, dt, state, pars, rng)
  }
  packer$pack(state)
}


sir_step <- function(time, dt, state, pars, rng) {
  p_SI <- 1 - exp(-pars$beta * state$I / pars$N * dt)
  p_IR <- rep(1 - exp(-pars$gamma * dt), length(state$I))
  n_SI <- monty_random_binomial(state$S, p_SI, rng)
  n_IR <- monty_random_binomial(state$I, p_IR, rng)
  cases <- if (time %% 1 == 0) 0 else state$cases
  list(S = state$S - n_SI,
       I = state$I + n_SI - n_IR,
       R = state$R + n_IR,
       cases = cases + n_SI)
}


dust_resample_weight <- function(w, u) {
  n <- length(w)
  uu <- u / n + seq(0, by = 1 / n, length.out = n)
  findInterval(uu, cumsum(w / sum(w))) + 1L
}
