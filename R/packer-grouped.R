##' Build a grouped version of [monty_packer()] with the same basic
##' idea; convert between a vector representation of some group of
##' numbers to a named list of structured data, but with an extra
##' twist: this time the unstructured vector of numbers contains
##' values that correspond to multiple *groups* and some are shared
##' across groups while others vary between groups.  This function
##' does a lot of bookkeeping in a relatively short amount of code,
##' so you should be familiar with the ideas in [monty_packer()]
##' before continuing.
##'
##' Recall from [monty_packer()] that our original problem was to take
##' an unstructured vector like
##'
##' ```
##' | 1  2  3  4  5  6  7  |
##' | a  b  c  d1 d2 d3 d4 |
##' ```
##'
##' and **unpack it** into a structured list like
##'
##' ```r
##' list(a = 1, b = 2, c = 3, d = 4:7)
##' ```
##'
##' Our aim here is to do the same but to allow some of these values (say
##' `b` and `c`) to be shared (constant) over groups while the others
##' (`a` and `d`) to vary by group.  So for groups `x` and `y` we might
##' try and create something like
##'
##' ```r
##' list(
##'   list(x = list(a = 3, b = 1, c = 2, d = 4:7),
##'        y = list(a = 8, b = 1, c = 2, d = 9:12))
##' ```
##'
##' from a vector
##'
##' ```
##' | 1  2  3  4  5  6  7  8  9  10 11 12 |
##' | b  c  a  d1 d2 d3 d4 a  d1 d2 d3 d4 |
##' | xy xy x  x  x  x  x  y  y  y  y  y  |
##' ```
##'
##' @title Build a nested packer
##'
##' @param groups A character vector of group names.  These must not
##'   be present within any of your `scalar` or `array` arguments.
##'
##' @param shared Names of the elements in `scalar` and `array` that
##'   are shared among all groups.
##'
##' @inheritParams monty_packer
##'
##' @return An object of class `monty_packer_grouped`, which has the
##'   same elements as `monty_packer`, though with slightly different
##'   effects.
##'
##' * `names`: a function that returns a character vector of computed
##'   names; in the parameter packer context these are the names that
##'   your statistical model will use.
##'
##' * `groups`: A function that returns your group names (the `groups`
##'   argument as supplied)
##'
##' * `unpack`: A function for converting from an unstructured
##'   vector into a nested list.  Each element of this list is
##'   conceptually the same as the result of `unpack()` from
##'   [monty_packer()].
##'
##' * `pack`: The inverse to `unpack()` but less commonly performed.
##'   Convert a nested list into an unstructured vector.  Quite a lot
##'   of validation is required to make sure that the input has not
##'   been tampered with, and errors thrown while doing this
##'   validation may not be very interpretable.
##'
##' * `index`: The nested version of the `index()` function in
##'   [monty_packer()]. The outer list is over groups, and the inner
##'   list contains the position within the original unstructured
##'   vector where this value can be found.  It is not clear to us if
##'   this is a useful list.
##'
##' * `subset`: A function that might eventually allow subsetting a
##'   grouped packer.  Currently it just errors.
##'
##' @export
##' @examples
##' p <- monty_packer_grouped(c("x", "y"), c("a", "b", "c", "d", "e"),
##'                           shared = c("b", "c"))
##' p$names()
##' p$unpack(1:8)
monty_packer_grouped <- function(groups, scalar = NULL, array = NULL,
                                 fixed = NULL, process = NULL, shared = NULL) {
  assert_character(groups)
  n_groups <- length(groups)
  if (n_groups < 2) {
    cli::cli_abort("Expected at least two groups", arg = "groups")
  }
  if (!is.null(process)) {
    cli::cli_abort("'process' is not yet compatible with grouped packers")
  }

  if (!is.null(fixed)) {
    i <- names(fixed) %in% groups
    fixed <- list(shared = if (!all(i)) fixed[!i],
                  varied = if (any(i)) set_names(fixed[groups], groups))
  }

  p_base <- monty_packer(scalar, array, fixed = fixed$shared)

  assert_distinct_values(shared)

  nms <- names(p_base$index())
  err <- setdiff(shared, nms)
  if (length(err) > 0) {
    cli::cli_abort(
      paste("Unknown value{?s} in 'shared' not present in",
            "'scalar' or 'array': {squote(err)}"),
      arg = "shared")
  }
  varied <- setdiff(nms, shared)

  check <- list(scalar = scalar, array = names(array))
  for (nm in names(check)) {
    err <- intersect(groups, check[[nm]])
    if (length(err)) {
      cli::cli_abort(
        paste("'groups' must be distinct from '{nm}' but {squote(err)} was",
              "used in both"),
        arg = groups)
    }
  }

  if (!is.null(fixed$varied)) {
    nms_fixed <- unlist(lapply(fixed$varied, names), FALSE, FALSE)
    check <- list(scalar = scalar, array = names(array), groups = groups)
    for (nm in names(check)) {
      err <- intersect(nms_fixed, check[[nm]])
      if (length(err) > 0) {
        cli::cli_abort(
          paste("{cli::qty(length(err))}Group-varying fixed element",
                "{?name clashes/names clash} with '{nm}': {squote(err)}"))
      }
    }
  }

  d_shared <- p_base$subset(shared)
  d_varied <- p_base$subset(varied)

  n_groups <- length(groups)
  n_shared <- length(d_shared$index)
  n_varied <- length(d_varied$index)

  d_shared$index_packed <- seq_len(n_shared)
  d_varied$index_packed <- n_shared + seq_len(n_varied * n_groups)

  len <- n_shared + n_varied * n_groups
  nms <- names(p_base$index())

  ## Ideas here for names are:
  ##
  ## parameter<group>
  ## parameter{group}
  ## group$parameter
  ## group:parameter
  names_expanded <- c(d_shared$packer$names(),
                      outer(d_varied$packer$names(), groups,
                            function(g, p) sprintf("%s<%s>", g, p)))

  unpack <- function(x) {
    if (!is.null(dim(x))) {
      cli::cli_abort(
        "Can't use unpack with matrix input and grouped packer yet")
    }
    if (length(x) != len) {
      cli::cli_abort(
        "Incorrect length input; expected {len} but given {length(x)}")
    }
    assert_named_with(x, names_expanded, required = FALSE)
    base <- c(set_names(vector("list", length(nms)), nms),
              fixed$shared)
    base[shared] <- d_shared$packer$unpack(x[d_shared$index_packed])
    ret <- rep(list(base), n_groups)
    x_varied <- matrix(x[d_varied$index_packed], n_varied, n_groups)
    for (i in seq_len(n_groups)) {
      ret[[i]][varied] <- d_varied$packer$unpack(x_varied[, i])
      if (!is.null(fixed$varied)) {
        ret[[i]][names(fixed$varied[[i]])] <- fixed$varied[[i]]
      }
    }
    names(ret) <- groups
    ret
  }

  pack <- function(p) {
    ## Where this throws errors they'll be hard to cope with, but I
    ## don't expect that this will be very useful yet - it's mostly
    ## here for completeness.
    assert_named_with(p, groups, required = TRUE)
    p_shared <- lapply(p, "[", shared)
    if (length(unique(p_shared)) != 1L) {
      cli::cli_abort("Shared values are not identical across groups")
    }
    p_varied <- lapply(p, "[", varied)
    x <- rep_len(NA_real_, len)
    x[d_shared$index_packed] <- d_shared$packer$pack(p_shared[[1L]])
    x[d_varied$index_packed] <-
      vapply(p_varied, d_varied$packer$pack, numeric(n_varied))
    x
  }

  idx <- unpack(seq_len(len))

  ## This would be really quite hard, and I'm not sure how effective?
  ## But we might want it later to support multistage simulation in
  ## dust2.
  subset <- function(...) {
    cli::cli_abort("subset() is not supported for grouped packers")
  }

  ret <- list(names = function() names_expanded,
              groups = function() groups,
              unpack = unpack,
              pack = pack,
              index = function() idx,
              subset = subset)
  class(ret) <- "monty_packer_grouped"
  ret
}

##' @export
print.monty_packer_grouped <- function(x, ...) {
  cli::cli_h1("<monty_packer_grouped>")
  cli::cli_alert_info(
    "Packing {length(x$names())} value{?s}: {squote(x$names())}")
  cli::cli_alert_info(
    "Packing {length(x$groups())} group{?s}: {squote(x$groups())}")
  cli::cli_alert_info(
    "Use '$pack()' to convert from a list to a vector")
  cli::cli_alert_info(
    "Use '$unpack()' to convert from a vector to a list")
  cli::cli_alert_info("See {.help monty_packer_grouped} for more information")
  invisible(x)
}
