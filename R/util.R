`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


check_vcv <- function(vcv, name = deparse(substitute(vcv)), call = NULL) {
  if (!is.matrix(vcv)) {
    cli::cli_abort("Expected '{name}' to be a matrix",
                   arg = name, call = call)
  }
  if (!isSymmetric(vcv)) {
    cli::cli_abort("Expected '{name}' to be symmetric",
                   arg = name, call = call)
  }
  if (!is_positive_definite(vcv)) {
    cli::cli_abort("Expected '{name}' to be positive definite",
                   arg = name, call = call)
  }
}


is_positive_definite <- function(x, tol = sqrt(.Machine$double.eps)) {
  ev <- eigen(x, symmetric = TRUE)
  all(ev$values >= -tol * abs(ev$values[1]))
}


mcstate_file <- function(path) {
  system.file(path, package = "mcstate2", mustWork = TRUE)
}


## Small helper to get R's RNG state; we can use this to check that
## the state was not updated unexpectedly.
get_r_rng_state <- function() {
  if (exists(".Random.seed", globalenv(), inherits = FALSE)) {
    get(".Random.seed", globalenv(), inherits = FALSE)
  } else {
    NULL
  }
}
