##' @export
print.monty_samples <- function(x, ...) {
  d <- dim(x$pars)
  n_pars <- d[[1]]
  n_samples <- d[[2]]
  n_chains <- d[[3]]
  cli::cli_h1(
    paste("<monty_samples: {n_pars} parameter{?s} x {n_samples} sample{?s}",
          "x {n_chains} chain{?s}>"))
  cli::cli_alert_info("Parameters: {squote(rownames(x$pars))}")

  target <- rbind(c("posterior", "as_draws_array"),
                  c("posterior", "as_draws_df"),
                  c("coda", "as.mcmc.list"))
  status <- suggested_package_status(target[, 1])
  status_cls <- c(missing = "danger", installed = "warning", loaded = "success")
  status_str <- status
  status_str[status == "installed"] <- "installed, but not loaded"
  target_str <- sprintf(
    "{.alert-%s %s::%s() {cli::col_grey('[package %s]')}}",
    status_cls[status], target[, 1], target[, 2], status_str)
  cli::cli_alert_info(
    "Conversion to other types is possible:")
  cli::cli_bullets(set_names(target_str, ">"))
  if (!is.null(x$restart)) {
    cli::cli_alert_info(
      "These samples can be restared with {.help monty_sample_continue}")
  }
  if (!is.null(x$observations)) {
    cli::cli_alert_info(
      "These samples have associated observations")
  }

  cli::cli_alert_info(
    paste('See {.help monty_sample} and {.run vignette("samples")} for more',
          "information"))

  invisible(x)
}


##' @exportS3Method posterior::as_draws_array
as_draws_array.monty_samples <- function(x, ...) {
  arr <- aperm(x$pars, c(2, 3, 1))
  names(dimnames(arr)) <- c("chain", "iteration", "variable")
  class(arr) <- c("draws_array", "draws", "array")
  arr
}

##' @exportS3Method posterior::as_draws_df
as_draws_df.monty_samples <- function(x, ...) {
  posterior::as_draws_df(as_draws_array.monty_samples(x))
}


##' @exportS3Method coda::as.mcmc.list
as.mcmc.list.monty_samples <- function(x, ...) {
  n_chains <- dim(x$pars)[[3]]
  coda::mcmc.list(lapply(seq_len(n_chains), function(i) {
    coda::mcmc(t(array_drop(x$pars[, , i, drop = FALSE], 3)))
  }))
}
