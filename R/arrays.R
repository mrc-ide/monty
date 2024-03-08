array_bind <- function(..., arrays = list(...), on = NULL,
                       before = NULL, after = NULL, call = NULL) {
  if (length(arrays) == 0) {
    cli::cli_abort("Must provide at least one array", call = call)
  }
  d <- check_ranks_same(
    arrays,
    "Can't bind these arrays together, they do not have the same rank",
    call)
  r <- nrow(d)

  ## Allow one of on, before or after, then validate
  ## these inputs (this is a bunch of work).
  if ((!is.null(before)) + (!is.null(after)) + (!is.null(on)) > 1) {
    cli::cli_abort(
      "Only one of 'on', 'before' or 'after' may be given")
  }
  if (!is.null(after)) {
    if (after < 1 || after > r) {
      cli::cli_abort(
        "Invalid value for 'after' ({after}), must be in [1, {r}]",
        arg = "after", call = call)
    }
  } else if (!is.null(before)) {
    if (before < 1 || before > r) {
      cli::cli_abort(
        "Invalid value for 'before' ({before}), must be in [1, {r}]",
        arg = "before", call = call)
    }
  } else if (is.null(on)) {
    on <- r
  }

  ## Then we need to check the shared dimensions
  check_dimensions_same(
    d, on,
    "Can't bind these arrays together, their shared dimensions do not agree",
    call)

  ## Now, if we are not using 'on', we need to inflate all our arrays.
  ## We could do this within the logic below but it really complicates
  ## things.
  if (is.null(on)) {
    if (!is.null(after)) {
      arrays <- lapply(arrays, array_reshape,
                       after, c(d[after, 1], 1), call)
      on <- after + 1L
    } else {
      arrays <- lapply(arrays, array_reshape,
                       before, c(1, d[before, 1]), call)
      on <- before
    }
    r <- r + 1L
    d <- vapply(arrays, dim2, numeric(r))
  }

  n <- d[on, ]
  d <- d[, 1]
  d[[on]] <- sum(n)

  if (on == r) {
    ret <- array(unlist(arrays), d)
  } else {
    offset <- c(0, cumsum(n))
    ret <- array(arrays[[1]][[1]] * NA, d)
    idx <- array(seq_along(ret), d)
    for (i in seq_along(arrays)) {
      j <- seq_len(n[[i]]) + offset[[i]]
      k <- c(array_nth_dimension(idx, on, j))
      ret[k] <- c(arrays[[i]])
    }
  }

  dn <- dimnames(arrays[[1]])
  if (!is.null(dn)) {
    dn_this <- lapply(arrays, function(x) dimnames(x)[[on]])
    if (all(lengths(dn_this) > 0)) {
      dn[[on]] <- unlist(dn_this)
    } else {
      dn[on] <- list(NULL)
    }
    dimnames(ret) <- dn
  }

  ret
}


array_reshape <- function(x, i, d, call = NULL) {
  dx <- dim2(x)
  if (length(dx) < i) {
    cli::cli_abort(
      "array only has {length(dx)} dimension{?s}, can't update dimension {i}",
      call = call)
  }
  if (dx[[i]] != prod(d)) {
    dim_str <- paste(d, collapse = ", ")
    cli::cli_abort(
      paste("New dimensions ({dim_str}) imply dimension {i} has length",
            "{prod(d)} but found {dx[[i]]}"),
      call = call)
  }

  dn <- dimnames2(x)

  ## The actual reshape is easy:
  dim(x) <- append(dx[-i], d, i - 1L)

  ## Can't preserve dimension names on modified dimensions
  if (!is.null(dn)) {
    dimnames(x) <- append(dn[-i], rep(list(NULL), length(d)), i - 1L)
  }
  x
}


array_drop <- function(x, i, call = NULL) {
  dx <- dim2(x)
  rank <- length(dx)
  if (rank < max(i)) {
    cli::cli_abort(
      "Can't update dimension {max(i)}, array only has {rank} dimension{?s}",
      call = call)
  }
  if (!all(dx[i] == 1)) {
    err <- i[dx[i] != 1]
    ## I can't make cli's pluralisation work here easily, doing that
    ## manually:
    if (length(err) == 1L) {
      cli::cli_abort(
        "Can't drop dimension {err} as it is length {dx[err]}, not 1",
        call = call)
    } else {
      cli::cli_abort(
        "Can't drop dimensions {err} as they are length {dx[err]}, not 1",
        call = call)
    }
  }
  dn <- dimnames(x)
  dim(x) <- dim(x)[-i]
  if (!is.null(dn)) {
    if (rank == 2) {
      x <- c(x)
      names(x) <- dn[[-i]]
    } else {
      dimnames(x) <- dn[-i]
    }
  }
  x
}



array_nth_dimension <- function(x, k, i) {
  rank <- length(dim2(x))
  if (k < 1 || k > rank) {
    cli::cli_abort("'k' must be in [1, {rank}]")
  }
  expr <- switch(rank,
                 quote(x[]),                           # 1
                 quote(x[, , drop = FALSE]),           # 2
                 quote(x[, , , drop = FALSE]),         # 3
                 quote(x[, , , , drop = FALSE]),       # 4
                 quote(x[, , , , , drop = FALSE]),     # 5
                 quote(x[, , , , , , drop = FALSE]),   # 6
                 quote(x[, , , , , , , drop = FALSE]), # 7
                 cli::cli_abort("Unexpected rank")) # crazy stuff.
  expr[[k + 2]] <- quote(i)
  eval(expr)
}


check_dimensions_same <- function(d, ignore = NULL, message = NULL,
                                  call = NULL) {
  if (length(ignore) > 0) {
    d[ignore, ] <- 0
  }
  d_diff <- d - d[, 1] != 0
  if (any(d_diff)) {
    r <- nrow(d)
    err <- which(d_diff, TRUE)
    err_dim <- unname(seq_len(r)[err[, 1]])
    err_msg <- vcapply(split(err_dim, err[, 2]), paste, collapse = ", ")
    detail <- sprintf("array %s (dimension %s)",
                      names(err_msg), unname(err_msg))
    cli::cli_abort(
      c(message %||% "Incompatible dimension arrays",
        set_names(detail, "*")),
      call = call)
  }
}


check_ranks_same <- function(arrays, message = NULL, call = call) {
  d <- lapply(arrays, dim2)
  r <- lengths(d)
  err <- r != r[[1]]
  if (any(err)) {
    detail <- sprintf("array %d has rank %d", seq_along(r), r)
    cli::cli_abort(
      c(message %||% "Incompatible rank arrays",
        set_names(detail, "*")),
      call = call)
  }
  r <- r[[1]]
  matrix(unlist(d), r)
}
