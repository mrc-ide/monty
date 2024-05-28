assert_is <- function(x, what, name = deparse(substitute(x)), call = NULL) {
  if (!inherits(x, what)) {
    cli::cli_abort("Expected '{name}' to be a '{what}' object",
                   arg = name, call = call)
  }
  invisible(x)
}


assert_scalar <- function(x, name = deparse(substitute(x)), arg = name,
                          call = NULL)  {
  if (length(x) != 1) {
    cli::cli_abort(
      c("'{name}' must be a scalar",
        i = "{name} has length {length(x)}"),
      call = call, arg = arg)
  }
  invisible(x)
}


assert_logical <- function(x, name = deparse(substitute(x)),
                          arg = name, call = NULL) {
  if (!is.logical(x)) {
    cli::cli_abort("Expected '{name}' to be logical", arg = arg, call = call)
  }
  invisible(x)
}


assert_character <- function(x, name = deparse(substitute(x)),
                             arg = name, call = NULL) {
  if (!is.character(x)) {
    cli::cli_abort("Expected '{name}' to be character", call = call, arg = arg)
  }
  invisible(x)
}


assert_nonmissing <- function(x, name = deparse(substitute(x)),
                          arg = name, call = NULL) {
  if (anyNA(x)) {
    cli::cli_abort("Expected '{name}' to be non-NA", arg = arg, call = call)
  }
  invisible(x)
}


assert_scalar_logical <- function(x, name = deparse(substitute(x)),
                                  arg = name, call = NULL) {
  assert_scalar(x, name, arg = arg, call = call)
  assert_logical(x, name, arg = arg, call = call)
  assert_nonmissing(x, name, arg = arg, call = call)
}


assert_scalar_character <- function(x, name = deparse(substitute(x)),
                                    arg = name, call = NULL) {
  assert_scalar(x, name, arg = arg, call = call)
  assert_character(x, name, arg = arg, call = call)
  assert_nonmissing(x, name, arg = arg, call = call)
}


assert_named <- function(x, unique = FALSE, name = deparse(substitute(x)),
                         arg = name, call = NULL) {
  if (is.null(names(x))) {
    cli::cli_abort("'{name}' must be named", call = call, arg = arg)
  }
  if (unique && anyDuplicated(names(x))) {
    dups <- unique(names(x)[duplicated(names(x))])
    cli::cli_abort(
      c("'{name}' must have unique names",
        i = "Found {length(dups)} duplicate{?s}: {collapseq(dups)}"),
      call = call, arg = arg)
  }
  invisible(x)
}


assert_unique <- function(x, name = deparse(substitute(x)), arg = name,
                          call = call) {
  if (anyDuplicated(x)) {
    dups <- unique(x[duplicated(x)])
    cli::cli_abort(
      c("'{name}' must be unique",
        i = "Found {length(dups)} duplicate{?s}: {collapseq(dups)}"),
      call = call, arg = arg)
  }
  invisible(x)
}


assert_list <- function(x, name = deparse(substitute(x)), arg = name,
                        call = NULL) {
  if (!is.list(x)) {
    cli::cli_abort("Expected '{name}' to be a list",
                   arg = arg, call = call)
  }
  invisible(x)
}


match_value <- function(x, choices, name = deparse(substitute(x)), arg = name,
                        call = NULL) {
  assert_scalar_character(x, call = call, arg = arg)
  if (!(x %in% choices)) {
    choices_str <- paste(squote(choices), collapse = ", ")
    cli::cli_abort(c("'{name}' must be one of {choices_str}",
                     i = "Instead we were given '{x}'"), call = call,
                   arg = arg)
  }
  x
}
