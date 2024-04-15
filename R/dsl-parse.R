dsl_parse <- function(exprs) {
  exprs <- lapply(exprs, dsl_parse_expr)

  dsl_parse_check_duplicates(exprs)
  dsl_parse_check_usage(exprs)

  name <- vcapply(exprs, "[[", "name")
  parameters <- name[vcapply(exprs, "[[", "type") == "stochastic"]

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

  ## Here, the user has not provided a call to anything, or a call to
  ## something that is not recognised as a distribution.  We throw the
  ## same error in both cases, but with different contextual
  ## information in order to fix the error.
  if (!rlang::is_call(rhs, names(dsl_distributions))) {
    if (!rlang::is_call(rhs)) {
      detail <- c("x" = "rhs is not a function call")
    } else {
      detail <- c(i = paste("See ?'dsl-distributions' for details on",
                            "supported distributions"))
      distr_name <- as.character(rhs[[1]])
      dym <- near_match(distr_name, names(dsl_distributions))
      if (length(dym) > 0) {
        detail <- c(
          "i" = paste("Unknown distribution '{distr_name}', did you mean:",
                      "{squote(dym)}"),
          detail)
      }
    }
    dsl_parse_error(
      c("Expected rhs of '~' relationship to be a call to a distribution",
        detail),
      expr)
  }

  ## Next, match the arguments to the call, in order to
  distr_name <- as.character(rhs[[1]])
  args <- as.list(rhs[-1])
  candidates <- dsl_distributions[[distr_name]]
  match <- match_call(args, lapply(candidates, "[[", "args"))
  if (!match$success) {
    dsl_parse_error(
      c("Invalid call to '{distr_name}()'", match$error),
      expr)
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
       distribution = candidates[[match$index]],
       args = args[match$args],
       depends = depends,
       expr = expr)
}


dsl_parse_expr_assignment <- function(expr) {
  lhs <- expr[[2]]
  if (!rlang::is_symbol(lhs)) {
    ## TODO: once we support array expressions this will be relaxed a
    ## little to allow lhs to be 'symbol[index]'
    dsl_parse_error("Expected lhs of assignment to be a symbol", expr)
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


dsl_parse_error <- function(msg, expr, ..., envir = parent.frame(),
                            call = NULL) {
  cli::cli_abort(msg,
                 ...,
                 expr = expr,
                 footer = dsl_parse_error_show_context,
                 .envir = envir,
                 class = "mcstate2_parse_error",
                 call = call)
}


dsl_parse_error_show_context <- function(cnd, ...) {
  detail <- c(">" = "In expression",
              format_error_expr(cnd$expr))
  for (i in seq_along(cnd$context)) {
    detail <- c(detail,
                "",
                i = names(cnd$context)[[i]],
                format_error_expr(cnd$context[[i]]))
  }
  ## Annoyingly, there's no way of marking text as whitespace
  ## preserving within cli, so we need to do a substitution here for
  ## "nonbreaking space" which does ok.  We should also convert tabs
  ## to some number of spaces, probably.
  gsub(" ", "\u00a0", detail)
}


format_error_expr <- function(expr) {
  str <- attr(expr, "str", exact = TRUE)
  if (is.null(str)) {
    detail <- deparse(expr)
  } else {
    ## We can adjust the formatting here later, but this will
    ## hopefully be fairly nice for users.
    lines <- seq(attr(expr, "line"), length.out = length(str))
    detail <- sprintf("%s| %s", cli::col_grey(format(lines, width = 3)), str)
  }
  detail
}


dsl_parse_check_duplicates <- function(exprs) {
  type <- vcapply(exprs, "[[", "type")
  name <- vcapply(exprs, "[[", "name")
  i_err <- anyDuplicated(name)
  if (i_err > 0) {
    name_err <- name[[i_err]]
    i_prev <- which(name == name_err)[[1]]
    if (type[[i_err]] == type[[i_prev]]) {
      if (type[[i_err]] == "stochastic") {
        msg <- "Duplicated relationship '{name_err}'"
      } else {
        msg <- "Duplicated assignment '{name_err}'"
      }
    } else {
      if (type[[i_err]] == "stochastic") {
        msg <- "Relationship '{name_err}' shadows previous assignment"
      } else {
        msg <- "Assignment '{name_err}' shadows previous relationship"
      }
    }
    context <- list("Previous definition" = exprs[[i_prev]]$expr)
    dsl_parse_error(msg, exprs[[i_err]]$expr, context = context)
  }
}


dsl_parse_check_usage <- function(exprs) {
  name <- vcapply(exprs, "[[", "name")
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    err <- setdiff(e$depends, name[seq_len(i - 1)])
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
        context <- NULL
      }
      ## TODO: It would be nice to indicate that we want to highlight
      ## the variables 'err' here within the expression; that is
      ## probably something rlang can do for us as it does that with
      ## the 'arg' argument to rlang::abort already?
      dsl_parse_error("Invalid use of variable{?s} {squote(err)}",
                      e$expr, context = context)
    }
  }
}
