## The default of gradient_required = TRUE here helps with tests
dsl_parse <- function(exprs, gradient_required = TRUE, fixed = NULL,
                      domain = NULL, call = NULL) {
  exprs <- lapply(exprs, dsl_parse_expr, call)

  dat <- dsl_parse_arrays(exprs, fixed, call)
  
  #dsl_parse_check_duplicates(dat$exprs, call)
  #dsl_parse_check_fixed(dat$exprs, fixed, call)
  #dsl_parse_check_usage(dat$exprs, fixed, call)

  name <- vcapply(dat$exprs, function(x) x$lhs$name)
  parameters <- unique(name[vcapply(dat$exprs, "[[", "type") == "stochastic"])
  assigned <- setdiff(unique(name), parameters)

  if (!is.null(domain)) {
    domain <- validate_domain(domain, parameters, call = call)
  }

  adjoint <- dsl_parse_adjoint(parameters, dat$exprs, gradient_required)

  list(parameters = parameters, assigned = assigned, exprs = dat$exprs,
       arrays = dat$arrays, adjoint = adjoint, fixed = fixed, domain = domain)
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
  
  lhs <- dsl_parse_expr_stochastic_lhs(expr, call)
  
  rhs <- dsl_parse_expr_stochastic_rhs(expr, call)
  
  rhs$depends <- dsl_parse_expr_check_index_usage(
    rhs$depends, lhs$array, expr, call)

  ## Here we might check the arguments to the distribution functions,
  ## too, but that's also easy enough to do elsewhere.
  list(type = "stochastic",
       lhs = lhs,
       rhs = rhs,
       expr = expr)
}


dsl_parse_expr_stochastic_lhs <- function(expr, call) {
  lhs <- expr[[2]]
  
  is_array <- rlang::is_call(lhs, "[")
  if (is_array) {
    name <- deparse1(lhs[[2]])
    ## name <- dsl_parse_expr_check_lhs_name(lhs[[2]], special, is_array, src, call)
    array <- Map(dsl_parse_expr_check_lhs_index,
                 seq_len(length(lhs) - 2),
                 lhs[-(1:2)],
                 MoreArgs = list(name = name, expr = expr, call = call))
    depends <- join_dependencies(
      lapply(array, function(x)
        join_dependencies(lapply(x[c("at", "from", "to")], find_dependencies))))
  } else {
    name <- deparse1(lhs)
    ## name <- parse_expr_check_lhs_name(lhs, special, is_array, src, call)
    array <- NULL
    depends <- NULL
  }
  
  list(
    name = name,
    array = array,
    depends = depends)
}


dsl_parse_expr_stochastic_rhs <- function(expr, call) {
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
  
  list(depends = depends,
       distribution = res$value,
       expr = rhs)
}


dsl_parse_expr_assignment <- function(expr, call) {
  
  lhs <- dsl_parse_expr_assignment_lhs(expr, call)
  special <- lhs$special
  lhs$special <- NULL
  if (identical(special, "dim")) {
    lhs$name_data <- lhs$name
    lhs$name <- dsl_parse_dim_name(lhs$name)
    rhs <- dsl_parse_expr_assignment_rhs_dim(expr, call)
  } else {
    rhs <- dsl_parse_expr_assignment_rhs(expr, call)
  }
  
  rhs$depends <- dsl_parse_expr_check_index_usage(
    rhs$depends, lhs$array, src, call)
  
  ## I suspect we'll need to be quite restrictive about what
  ## expressions are possible, but for now nothing special is done.
  list(type = "assignment",
       special = special,
       lhs = lhs,
       rhs = rhs,
       expr = expr)
}


dsl_parse_expr_assignment_lhs <- function(expr, call) {
  lhs <- expr[[2]]
  
  if (rlang::is_call(lhs, "dim")) {
    special <- "dim"
    if (length(lhs) < 2) {
      dsl_parse_error(
        "Invalid call to 'dim()' on lhs; no variables given",
        "E105", expr, call)
    }
    dim_args <- rlang::call_args(lhs)
    if (any(nzchar(names(dim_args)))) {
      dsl_parse_error(
        "Invalid call to 'dim()' on lhs; arguments must be unnamed",
        "E105", expr, call)
    }
    lhs <- vcapply(dim_args, function(x) {
      if (!is.symbol(x)) {
        dsl_parse_error(
          "Invalid call to 'dim()' on lhs; '{deparse1(x)}' is not a symbol",
          "E105", expr, call)
      }
      # dsl_parse_expr_check_lhs_name(x, special, is_array, src, call)
      deparse1(x)
    })
    
    return(list(
      name_data = lhs[[1]], # may be more than one if dims are aliased
      names = unname(lhs),
      special = special))
  } else {
    special <- NULL
  }
  
  is_array <- rlang::is_call(lhs, "[")
  if (is_array) {
    name <- deparse1(lhs[[2]])
    ## name <- dsl_parse_expr_check_lhs_name(lhs[[2]], special, is_array, src, call)
    array <- Map(dsl_parse_expr_check_lhs_index,
                 seq_len(length(lhs) - 2),
                 lhs[-(1:2)],
                 MoreArgs = list(name = name, expr = expr, call = call))
    depends <- join_dependencies(
      lapply(array, function(x)
        join_dependencies(lapply(x[c("at", "from", "to")], find_dependencies))))
  } else {
    name <- deparse1(lhs)
    ## name <- parse_expr_check_lhs_name(lhs, special, is_array, src, call)
    array <- NULL
    depends <- NULL
  }
  
  list(name = name,
       special = special,
       array = array,
       depends = depends)
}


dsl_parse_expr_assignment_rhs <- function(expr, call) {
  rhs <- expr[[3]]
  
  depends <- all.vars(rhs)
  
  ## rhs <- parse_expr_usage(rhs, src, call)
  
  list(expr = rhs,
       depends = depends)
  
}


dsl_parse_expr_assignment_rhs_dim <- function(expr, call) {
  rhs <- expr[[3]]
  
  throw_no_stochastic <- function() {
    odin_parse_error(
      "Array extent cannot be stochastic",
      "E1039", src, call)
  }
  
  throw_invalid_rhs_dim <- function(err) {
    odin_parse_error(
      "Invalid function{?s} used on rhs of 'dim()': {squote(err)}",
      "E1043", src, call)
  }
  
  throw_bad_dim_arg <- function() {
    odin_parse_error(
      "When using 'dim()' on the right-hand-side, it takes only an array name",
      "E1066", src, call)
  }
  
  if (rlang::is_call(rhs, "c")) {
    value <- as.list(rhs[-1])
  } else {
    value <- list(rhs)
  }
  depends <- join_dependencies(lapply(value, find_dependencies))
  is_stochastic <- any(
    depends$functions %in% monty_dsl_distributions()$name)
  if (is_stochastic) {
    throw_no_stochastic()
  }
  
  if (rlang::is_call(rhs, "dim")) {
    if (!is.symbol(rhs[[2]])) {
      throw_bad_dim_arg()
    }
    return(list(type = "dim",
                value = rhs,
                depends = depends$variables))
  }
  allowed <- c("+", "-", "(", "length", "nrow", "ncol")
  err <- setdiff(depends$functions, allowed)
  if (length(err) > 0) {
    throw_invalid_rhs_dim(err)
  }
  list(type = "dim",
       value = value,
       depends = depends)
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
  err <- name %in% names(fixed)
  if (any(err)) {
    eq <- exprs[[which(err)[[1]]]]
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


dsl_parse_dim_name <- function(name) {
    sprintf("dim_%s", name)
}


dsl_parse_expr_check_lhs_index <- function(name, dim, index, expr, call) {
  ret <- dsl_parse_index(name, dim, index)
  
  if (is.null(ret)) {
    odin_parse_error(
      "Invalid value for array index lhs",
      "E1026", src, call)
  }
  
  ## We'll need to repeat most, but not all, of these checks when
  ## validating indicies used in sum/prod on the *rhs* but we will do
  ## it again as the checks are simple and the error messages need to
  ## reflect the context.
  if (any(lengths(ret$depends) > 0)) {
    if (":" %in% ret$depends$functions) {
      ## Previously in odin1 we tried to help disambiguate some calls
      ## in the error message; we might want to put that back in at
      ## some point, but it's not a big priority, most of the time
      ## this is pretty simple.
      odin_parse_error(
        c("Invalid use of range operator ':' on lhs of array assignment",
          paste("If you use ':' as a range operator on the lhs of an",
                "assignment into an array, then it must be the outermost",
                "call, for e.g, {.code (a + 1):(b + 1)}, not",
                "{.code 1 + (a:b)}")),
        "E1022", src, call)
    }
    allowed <- c("+", "-", "(", ":", "length", "nrow", "ncol")
    err <- setdiff(ret$depends$functions, allowed)
    if (length(err) > 0) {
      odin_parse_error(
        "Invalid function{?s} used in lhs of array assignment: {squote(err)}",
        "E1023", src, call)
    }
    if ("-" %in% ret$depends$functions && uses_unary_minus(index)) {
      odin_parse_error(
        "Invalid use of unary minus in lhs of array assignment",
        "E1024", src, call)
    }
    err <- intersect(INDEX, ret$depends$variables)
    if (length(err) > 0) {
      odin_parse_error(
        paste("Invalid use of special variable{?s} in lhs of array",
              "assignment: {squote(err)}"),
        "E1025", src, call)
    }
  }
  
  ret$depends <- NULL
  ret
}


dsl_parse_index <- function(name_data, dim, value) {
  name_index <- INDEX[[dim]]
  if (rlang::is_missing(value)) {
    to <- call("dsl_dim", name_data, dim)
    list(name = name_index, type = "range", from = 1, to = to, depends = NULL)
  } else if (rlang::is_call(value, ":")) {
    from <- value[[2]]
    to <- value[[3]]
    depends <- join_dependencies(list(find_dependencies(from),
                                      find_dependencies(to)))
    list(name = name_index, type = "range", from = from, to = to,
         depends = depends)
  } else if (is.language(value) || is.numeric(value)) {
    depends <- find_dependencies(value)
    list(name = name_index, type = "single", at = value, depends = depends)
  } else {
    NULL
  }
}


dsl_parse_expr_check_index_usage <- function(variables, array, expr, call) {
  index_used <- intersect(INDEX, variables)
  if (length(index_used) > 0) {
    n <- length(array)
    err <- if (n == 0) index_used else intersect(index_used, INDEX[-seq_len(n)])
    if (length(err) > 0) {
      v <- err[length(err)]
      i <- match(v, INDEX)
      odin_parse_error(
        c("Invalid index access used on rhs of equation: {squote(err)}",
          i = paste("Your lhs has only {n} dimension{?s}, but index '{v}'",
                    "would require {match(v, INDEX)}")),
        "E1021", expr, call)
    }
    ## index variables are not real dependencies, so remove them:
    variables <- setdiff(variables, INDEX)
  }
  variables
}


INDEX <- c("i", "j", "k", "l", "i5", "i6", "i7", "i8")
