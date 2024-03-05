assert_is <- function(x, what, name = deparse(substitute(x)), call = NULL) {
  if (!inherits(x, what)) {
    cli::cli_abort("Expected '{name}' to be a '{what}' object",
                   arg = name, call = call)
  }
  invisible(x)
}
