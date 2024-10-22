assert_distinct_values <- function(x, name = deparse(substitute(x)),
                                   arg = name, call = parent.frame()) {
  dups <- duplicate_values(x)
  if (length(dups) > 0) {
    cli::cli_abort(
      c("Elements of '{name}' must be unique",
        i = "Found {length(dups)} duplicate{?s}: {squote(dups)}"),
      arg = arg)
  }
  invisible(x)
}


assert_named_with <- function(x, expected, required = FALSE,
                              arg = deparse(substitute(x)),
                              type = "names",
                              call = parent.frame()) {
  nms <- switch(type,
                "names" = names(x),
                "row names" = rownames(x))
  ok <- identical(nms, expected) || (is.null(nms) && !required)
  if (!ok) {
    cli::cli_abort("Unexpected {type} for '{arg}'",
                   arg = arg, call = call)
  }
  invisible(x)
}
