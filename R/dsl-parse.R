dsl_parse <- function(exprs) {
  exprs <- lapply(exprs, dsl_parse_expr)

  type <- vcapply(exprs, "[[", "type")
  parameters <- vcapply(exprs[type == "stochastic"], "[[", "name")
  if (anyDuplicated(parameters)) {
    ## TODO: much better error here.
    cli::cli_abort("Duplicated parameters")
  }

  ## TODO: check arguments to calls, perhaps simplest to disallow for now?

  list(parameters = parameters, exprs = exprs)
}


dsl_parse_expr <- function(expr) {
  if (rlang::is_call(expr, "~")) {
    dsl_parse_expr_stochastic(expr)
  } else if (rlang::is_call(expr, c("<-", "="))) {
    dsl_parse_expr_assignment(expr)
  } else {
    dsl_parse_error(
      "Unhandled expression; expected something involving '~' or '<-'",
      expr)
  }
}


dsl_parse_expr_stochastic <- function(expr) {
  lhs <- expr[[2]]
  if (!rlang::is_symbol(lhs)) {
    ## TODO: once we support array expressions this will be relaxed a
    ## little to allow lhs to be 'symbol[index]'
    dsl_parse_error("Expected lhs of '~' relationship to be a symbol", expr)
  }
  rhs <- expr[[3]]

  ## TODO: this will be derived from source of truth soon:
  supported <- c("Normal", "Exponential", "Uniform")
  if (!rlang::is_call(rhs, supported)) {
    dsl_parse_error(
      "Expected rhs of '~' relationship to be a call to a distribution")
  }

  ## Here we might check the arguments to the distribution functions,
  ## too, but that's also easy enough to do elsewhere.
  list(type = "stochastic",
       name = as.character(lhs),
       distribution = as.character(rhs[[1]]),
       args = as.list(rhs[-1]),
       expr = expr)
}


dsl_parse_expr_assignment <- function(expr) {
  lhs <- expr[[2]]
  if (!rlang::is_symbol(lhs)) {
    ## TODO: once we support array expressions this will be relaxed a
    ## little to allow lhs to be 'symbol[index]'
    dsl_parse_error("Expected lhs of assignment to be a symbol", expr)
  }
  list(type = "assignment",
       name = as.character(lhs),
       expr = expr)
}


dsl_parse_error <- function(msg, expr) {
  str <- attr(expr, "str", exact = TRUE)
  if (is.null(str)) {
    detail <- deparse(expr)
  } else {
    ## We can adjust the formatting here later, but this will
    ## hopefully be fairly nice for users.
    lines <- seq(attr(expr, "line"), length.out = length(str))
    detail <- sprintf("%s| %s", cli::col_grey(format(lines)), str)
  }
  ## Using rlang::abort directly because whitespace will be important
  ## here.
  ##
  ## The other way to do this is to implement cnd_body methods for
  ## this class, but the effect will be similar.
  rlang::abort(c(msg, set_names(detail, " ")), mcstate2_expr = expr,
               class = "mcstate2_parse_error",
               call = NULL)
}
