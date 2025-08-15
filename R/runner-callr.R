##' Run MCMC chains in parallel (at the same time).  This runner uses
##' the `callr` package to distribute your chains over a number of
##' worker processes on the same machine.  If you have used `mcstate`,
##' this is the same as "worker" processes.  Unless your chains take a
##' few seconds to run, this will be slower than running with the
##' default serial runner ([monty_runner_serial]), however for long
##' running chains, the speed-up will typically scale with workers
##' added, so long as your chains divide neatly over workers.
##'
##' @title Run MCMC chains in parallel with `callr`
##'
##' @param n_workers The number of workers to use.  This should be no
##'   larger than the number of chains (though this is harmless) and
##'   no larger than the total number of cores available on your
##'   computer.  Ideally the number of chains you want to run is a
##'   multiple of this number (for example, if you had 8 chains, then
##'   1, 2, 4, and 8 are good choices of `n_workers`, and 7 workers
##'   will likely be no faster than 4).
##'
##' @inheritParams monty_runner_serial
##'
##' @return A runner of class `monty_runner` that can be passed to
##'   [monty_sample()]
##'
##' @export
monty_runner_callr <- function(n_workers, progress = NULL) {
  assert_scalar_size(n_workers, allow_zero = FALSE)
  progress <- show_progress_bar(progress)
  env <- new.env(parent = emptyenv())

  launch <- function(session_id) {
    pending <- which(env$status == "pending")
    if (length(pending) == 0) {
      return()
    }
    chain_id <- pending[[1]]
    env$sessions[[session_id]] <- callr::r_bg(
      function(id, path) monty::monty_sample_manual_run(id, path, "simple"),
      list(chain_id, env$path))
    env$target[[session_id]] <- chain_id
    env$status[[chain_id]] <- "running"
  }

  ## callr::poll will poll, with a timeout, all our processes.
  ## There's not much downside to a long poll because if they *are*
  ## ready then they will return instantly. However, the process
  ## will only be interruptable each time the timeout triggers, so
  ## use 1000 here (1s).
  step <- function(timeout = 1000) {
    res <- callr::poll(env$sessions, timeout)
    is_done <- vcapply(res, "[[", "process") == "ready"
    if (any(is_done)) {
      for (session_id in which(is_done)) {
        chain_id <- env$target[[session_id]]
        env$result_path[[chain_id]] <-
          callr_safe_result(env$sessions[[session_id]])
        env$status[[chain_id]] <- "done"
        env$n_steps_progress[[chain_id]] <- env$n_steps
        launch(session_id)
      }
    }

    if (progress != "none") {
      has_stderr <- vcapply(res, "[[", "error") == "ready" & !is_done
      if (any(has_stderr)) {
        for (session_id in which(has_stderr)) {
          stderr <- env$sessions[[session_id]]$read_error_lines()
          res <- parse_progress_bar_simple(stderr)
          if (!is.null(res)) {
            env$n_steps_progress[[res$chain_id]] <- res$step
          }
        }
      }
      env$progress$update(env$chain_ids, env$n_steps_progress)
    }

    all(env$status == "done")
  }

  loop <- function(path, n_workers, n_chains, steps, progress) {
    pb <- progress_bar(n_chains, steps$total, progress, show_overall = TRUE)
    n_workers <- min(n_chains, n_workers)
    env$path <- path
    env$sessions <- vector("list", n_workers)
    env$target <- rep(NA_integer_, n_workers)
    env$status <- rep("pending", n_chains)
    env$chain_ids <- seq_len(n_chains)
    env$result_path <- rep(NA_character_, n_chains)
    env$n_steps <- steps$total
    env$n_steps_progress <- rep(0, n_chains)
    env$progress <- pb
    for (session_id in seq_len(n_workers)) {
      launch(session_id)
    }
    with_progress_fail_on_error(
      pb,
      while (!step()) {
      })

    res <- lapply(env$result_path, readRDS)
    unlink(env$path, recursive = TRUE)
    res
  }

  run <- function(pars, model, sampler, steps, rng) {
    seed <- unlist(lapply(rng, function(r) monty_rng_state(r)))
    n_chains <- length(rng)
    path <- tempfile()
    sample_manual_prepare(
      model = model, sampler = sampler, steps = steps, path = path,
      initial = pars, n_chains = n_chains,
      seed = seed)
    loop(path, n_workers, n_chains, steps, progress)
  }

  continue <- function(state, model, sampler, steps) {
    restart <- list(state = state,
                    model = model,
                    sampler = sampler)
    n_chains <- n_chains_from_state(state)
    path <- tempfile()
    monty_sample_manual_prepare_continue(
      list(state = state, restart = restart), steps, path, "nothing")
    loop(path, n_workers, n_chains, steps, progress)
  }

  monty_runner("callr",
               "monty_runner_callr",
               run,
               continue)
}
