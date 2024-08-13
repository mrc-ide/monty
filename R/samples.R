##' @export
print.mcstate_samples <- function(x, ...) {
  d <- dim(x$pars)
  n_pars <- d[[1]]
  n_samples <- d[[2]]
  n_chains <- d[[3]]
  cli::cli_h1(
    paste("<mcstate_samples: {n_pars} parameter{?s} x {n_samples} sample{?s}",
          "x {n_chains} chain{?s}>"))
  cli::cli_alert_info("Parameters: {squote(rownames(x$pars))}")
  cli::cli_alert_info(
    "Conversion to other types is possible:")

  ## Later, we can render something indicating if these can actually
  ## be used basd on other packages being available.
  targets <- c("posterior::as_draws_array",
               "posterior::as_draws_df",
               "coda::as.mcmc.list")
  cli::cli_bullets(set_names(sprintf("%s()", targets), ">"))
  invisible(x)
}


##' @exportS3Method posterior::as_draws_array
as_draws_array.mcstate_samples <- function(x, ...) {
  arr <- aperm(x$pars, c(2, 3, 1))
  names(dimnames(arr)) <- c("chain", "iteration", "variable")
  class(arr) <- c("draws_array", "draws", "array")
  arr
}

##' @exportS3Method posterior::as_draws_df
as_draws_df.mcstate_samples <- function(x, ...) {
  posterior::as_draws_df(as_draws_array.mcstate_samples(x))
}


##' @exportS3Method coda::as.mcmc.list
as.mcmc.list.mcstate_samples <- function(x, ...) {
  n_chains <- dim(x$pars)[[3]]
  coda::mcmc.list(lapply(seq_len(n_chains), function(i) {
    coda::mcmc(t(array_drop(x$pars[, , i, drop = FALSE], 3)))
  }))
}
