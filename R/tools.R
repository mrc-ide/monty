##' Trace calls to R's random-number-generating functions, to detect
##' unexpected use of random number generation outside of monty's
##' control.
##'
##' @title Trace random number calls
##'
##' @param code Code to run with tracing on
##'
##' @param max_calls Maximum number of calls to report.  The default
##'   is 5
##'
##' @param show_stack Logical, indicating if we should show the stack
##'   at the point of the call
##'
##' @return The result of evaluating `code`
##'
##' @export
with_trace_random <- function(code, max_calls = 5, show_stack = FALSE) {
  fns <- list("set.seed",
              "sample",
              "rbeta",
              "rbinom",
              "rcauchy",
              "rchisq",
              "rexp",
              "rf",
              "rgamma",
              "rgeom",
              "rhyper",
              "rlnorm",
              "rmultinom",
              "rnbinom",
              "rnorm",
              "rpois",
              "rt",
              "runif",
              "rweibull")

  env <- new.env()
  env$n_calls <- 0
  here <- rlang::current_env()

  report <- function(fn) {
    force(fn)
    function() {
      env$n_calls <- env$n_calls + 1
      if (env$n_calls <= max_calls) {
        cli::cli_alert_warning("Detected call to '{fn}()'")
        if (show_stack) {
          tr <- rlang::trace_back(bottom = 2, top = here)
          message(paste(format(tr), collapse = "\n"))
        }
      }
    }
  }

  suppressMessages({
    for (fn in fns) {
      trace(fn, report(fn), print = FALSE)
    }
  })
  on.exit({
    if (env$n_calls > 0) {
      cli::cli_alert_warning(
        "A total of {env$n_calls} call{?s} to rng-using functions found")
    }
    suppressMessages({
      for (fn in fns) {
        untrace(fn)
      }
    })
  })
  code
}
