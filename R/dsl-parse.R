## The default of gradient_required = TRUE here helps with tests
dsl_parse <- function(exprs, gradient_required = TRUE, fixed = NULL,
                      call = NULL) {
  exprs <- lapply(exprs, dsl_parse_expr, call)

  dsl_parse_check_duplicates(exprs, call)
  dsl_parse_check_fixed(exprs, fixed, call)
  dsl_parse_check_usage(exprs, fixed, call)

  name <- vcapply(exprs, "[[", "name")
  parameters <- name[vcapply(exprs, "[[", "type") == "stochastic"]

  adjoint <- dsl_parse_adjoint(parameters, exprs, gradient_required)

  list(parameters = parameters, exprs = exprs, adjoint = adjoint, fixed = fixed)
}


dsl_parse_expr <- function(expr, call) {
  if (rlang::is_call(expr, "~")) {
    dsl_parse_expr_stochastic(expr, call)
  } else if (rlang::is_call(expr, c("<-", "="))) {
    dsl_parse_expr_assignment(expr, call)
  } else {
    dsl_parse_error(
      "Unhandled expression; expected something involving '~' or '<-'",
      "E101", expr, call)
  }
}


dsl_parse_expr_stochastic <- function(expr, call) {
  lhs <- expr[[2]]
  if (!rlang::is_symbol(lhs)) {
    ## TODO: once we support array expressions this will be relaxed a
    ## little to allow lhs to be 'symbol[index]'
    dsl_parse_error("Expected lhs of '~' relationship to be a symbol",
                    "E102", expr, call)
  }
  rhs <- expr[[3]]

  res <- monty_dsl_parse_distribution(rhs)

  if (!res$success) {
    dsl_parse_error(res$error, "E103", expr, call)
  }

  ## This probably requires a little more care in order to know that
  ## we're not picking up too much or too little.  I'm not sure that
  ## we can cope with every expression here too as I think we also
  ## need to be able to invert the expressions?
  depends <- all.vars(rhs)

  ## Here we might check the arguments to the distribution functions,
  ## too, but that's also easy enough to do elsewhere.
  list(type = "stochastic",
       name = as.character(lhs),
       distribution = res$value,
       depends = depends,
       expr = expr)
}


dsl_parse_expr_assignment <- function(expr, call) {
  lhs <- expr[[2]]
  if (!rlang::is_symbol(lhs)) {
    ## TODO: once we support array expressions this will be relaxed a
    ## little to allow lhs to be 'symbol[index]'
    dsl_parse_error("Expected lhs of assignment to be a symbol",
                   "E104", expr, call)
  }
  rhs <- expr[[3]]
  ## I suspect we'll need to be quite restrictive about what
  ## expressions are possible, but for now nothing special is done.
  depends <- all.vars(rhs)
  list(type = "assignment",
       name = as.character(lhs),
       depends = depends,
       rhs = rhs,
       expr = expr)
}


dsl_parse_check_duplicates <- function(exprs, call) {
  type <- vcapply(exprs, "[[", "type")
  name <- vcapply(exprs, "[[", "name")
  i_err <- anyDuplicated(name)
  if (i_err > 0) {
    name_err <- name[[i_err]]
    i_prev <- which(name == name_err)[[1]]
    if (type[[i_err]] == type[[i_prev]]) {
      if (type[[i_err]] == "stochastic") {
        msg <- "Duplicated relationship '{name_err}'"
        code <- "E201"
      } else {
        msg <- "Duplicated assignment '{name_err}'"
        code <- "E202"
      }
    } else {
      if (type[[i_err]] == "stochastic") {
        msg <- "Relationship '{name_err}' shadows previous assignment"
        code <- "E203"
      } else {
        msg <- "Assignment '{name_err}' shadows previous relationship"
        code <- "E204"
      }
    }
    context <- list("Previous definition" = exprs[[i_prev]]$expr)
    dsl_parse_error(msg, code, exprs[[i_err]]$expr, call, context = context)
  }
}


dsl_parse_check_fixed <- function(exprs, fixed, call) {
  if (is.null(fixed)) {
    return()
  }

  name <- vcapply(exprs, "[[", "name")
  err <- match(name, names(fixed))
  if (length(err) > 0) {
    eq <- exprs[[err[[1]]]] # just first is easiest to report on
    dsl_parse_error(
      "Value '{eq$name}' in 'fixed' is shadowed by {eq$type}",
      "E207", eq$expr, call)
  }
}


dsl_parse_check_usage <- function(exprs, fixed, call) {
  name <- vcapply(exprs, "[[", "name")
  names_fixed <- names(fixed)
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    err <- setdiff(e$depends, c(name[seq_len(i - 1)], names_fixed))
    if (length(err) > 0) {
      ## Out of order:
      out_of_order <- intersect(name, err)
      if (length(out_of_order) > 0) {
        context <- lapply(exprs[name %in% out_of_order], "[[", "expr")
        names(context) <- sprintf("'%s' is defined later:", out_of_order)
      } else {
        ## Could also tell the user about variables found in the
        ## calling env, but that requires detecting and then passing
        ## through the correct environment.
        ##
        ## Could also tell about near misses.
        context <- NULL
      }
      ## TODO: It would be nice to indicate that we want to highlight
      ## the variables 'err' here within the expression; that is
      ## probably something rlang can do for us as it does that with
      ## the 'arg' argument to rlang::abort already?
      dsl_parse_error("Invalid use of variable{?s} {squote(err)}",
                      "E205", e$expr, call, context = context)
    }
  }
}
