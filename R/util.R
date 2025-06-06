`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


check_vcv <- function(vcv, allow_3d = FALSE, name = deparse(substitute(vcv)),
                      call = NULL) {
  if (allow_3d && length(dim(vcv)) == 3) {
    check_vcv_array(vcv, name, call = environment())
  } else if (is.matrix(vcv)) {
    check_vcv_matrix(vcv, name, call = environment())
  } else {
    what <- if (allow_3d) "matrix or 3d array" else "matrix"
    cli::cli_abort("Expected a {what} for '{name}'", call = call)
  }
}


check_vcv_array <- function(vcv, name, call) {
  len <- dim(vcv)[[3]]
  n <- nrow(vcv)
  if (len == 0) {
    cli::cli_abort(
      "At least one vcv required within a vcv array",
      call = call)
  }
  for (i in seq_len(len)) {
    m <- array(vcv[, , i, drop = FALSE], dim(vcv)[1:2])
    check_vcv_matrix(m, name = sprintf("%s[, , %d]", name, i), call = call)
  }
}


check_vcv_matrix <- function(vcv, name, call) {
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


monty_file <- function(path) {
  system.file(path, package = "monty", mustWork = TRUE)
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


vlapply <- function(...) {
  vapply(..., FUN.VALUE = TRUE)
}


vnapply <- function(...) {
  vapply(..., FUN.VALUE = 1)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


rbind_list <- function(x) {
  stopifnot(all(vlapply(x, is.matrix)))
  if (length(x) == 1) {
    return(x[[1]])
  }
  nc <- unique(vnapply(x, ncol))
  stopifnot(length(nc) == 1)
  do.call("rbind", x)
}


squote <- function(x) {
  sprintf("'%s'", x)
}


dim2 <- function(x) {
  dim(x) %||% length(x)
}


"dim2<-" <- function(x, value) {
  if (length(value) > 1) {
    dim(x) <- value
  }
  x
}


dimnames2 <- function(x) {
  if (!is.null(dim(x))) {
    dimnames(x)
  } else {
    nms <- names(x)
    if (is.null(nms)) NULL else list(nms)
  }
}


set_names <- function(x, nms) {
  if (length(nms) == 1 && length(x) != 1) {
    nms <- rep_len(nms, length(x))
  }
  names(x) <- nms
  x
}


all_same <- function(x) {
  if (length(x) < 2) {
    return(TRUE)
  }
  all(vlapply(x[-1], identical, x[[1]]))
}


as_function <- function(args, body, envir) {
  if (!rlang::is_call(body, "{")) {
    body <- rlang::call2("{", !!!body)
  }
  as.function(c(args, body), envir = envir)
}


near_match <- function(x, possibilities, threshold = 2, max_matches = 5) {
  i <- tolower(x) == tolower(possibilities)
  if (any(i)) {
    possibilities[i]
  } else {
    d <- set_names(drop(utils::adist(x, possibilities, ignore.case = TRUE)),
                   possibilities)
    utils::head(names(sort(d[d <= threshold])), max_matches)
  }
}


duplicate_values <- function(x) {
  if (!anyDuplicated(x)) {
    return(x[integer(0)])
  }
  unique(x[duplicated(x)])
}


collapseq <- function(x) {
  paste(squote(x), collapse = ", ")
}


## This is definitely possible with rlang, but I am not sure how.
substitute_ <- function(expr, env) {
  eval(substitute(substitute(y, env), list(y = expr)))
}


suggested_package_status <- function(pkg) {
  status <- ifelse(pkg %in% loadedNamespaces(), "loaded", "missing")
  i <- status == "missing"
  if (any(i)) {
    status[i] <- ifelse(pkg[i] %in% .packages(TRUE), "installed", "missing")
  }
  set_names(status, pkg)
}


dir_create <- function(path) {
  dir.create(path, FALSE, TRUE)
}


is_directory <- function(path) {
  file.info(path, extra_cols = FALSE)$isdir
}


is_empty_directory <- function(path) {
  is_directory(path) && length(dir(path, all.files = TRUE, no.. = TRUE)) == 0
}


## I've seen rare cases where the processx poll says that the process
## is ready, but the process is still alive (shutting down).  So we
## give a short grace window here.
callr_safe_result <- function(rs, grace = 2, dt = 0.1) {
  for (i in seq(0, grace, by = dt)) {
    if (!rs$is_alive()) {
      break
    }
    Sys.sleep(dt)
  }
  ## Failure here will throw and propagate nicely to the controlling
  ## process.
  rs$get_result()
}


last <- function(x) {
  x[[length(x)]]
}


is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}
