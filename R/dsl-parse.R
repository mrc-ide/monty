## The default of gradient_required = TRUE here helps with tests
dsl_parse <- function(exprs, gradient_required = TRUE, fixed = NULL,
                      domain = NULL, call = NULL) {
  exprs <- lapply(exprs, dsl_parse_expr, call)

  arrays <- dsl_parse_arrays(exprs, fixed, call)
  
  exprs <- dsl_parse_check_system(exprs, arrays, fixed, call)
  
  name <- vcapply(exprs, function(x) x$lhs$name)
  parameters <- unique(name[vcapply(exprs, "[[", "type") == "stochastic"])
  assigned <- setdiff(unique(name), parameters)
  
  packer <- dsl_packer(parameters, arrays)

  if (!is.null(domain)) {
    domain <- validate_domain(domain, packer$names(), call = call)
  }

  adjoint <- dsl_parse_adjoint(parameters, exprs, gradient_required)

  list(parameters = parameters, assigned = assigned, packer = packer,
       exprs = exprs, arrays = arrays, adjoint = adjoint,
       fixed = fixed, domain = domain)
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
  special <- "~"
  if (is_array) {
    name <- 
      dsl_parse_expr_check_lhs_name(lhs[[2]], special, is_array, expr, call)
    array <- Map(dsl_parse_expr_check_lhs_index,
                 seq_len(length(lhs) - 2),
                 lhs[-(1:2)],
                 MoreArgs = list(name = name, expr = expr, call = call))
    depends <- join_dependencies(
      lapply(array, function(x)
        join_dependencies(lapply(x[c("at", "from", "to")], find_dependencies))))
  } else {
    name <- dsl_parse_expr_check_lhs_name(lhs, special, is_array, expr, call)
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
    rhs$depends, lhs$array, expr, call)
  
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
    is_array <- FALSE
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
      dsl_parse_expr_check_lhs_name(x, special, is_array, expr, call)
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
    name <- 
      dsl_parse_expr_check_lhs_name(lhs[[2]], special, is_array, expr, call)
    array <- Map(dsl_parse_expr_check_lhs_index,
                 seq_len(length(lhs) - 2),
                 lhs[-(1:2)],
                 MoreArgs = list(name = name, expr = expr, call = call))
    depends <- join_dependencies(
      lapply(array, function(x)
        join_dependencies(lapply(x[c("at", "from", "to")], find_dependencies))))
  } else {
    name <- dsl_parse_expr_check_lhs_name(lhs, special, is_array, expr, call)
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
  
  list(expr = rhs,
       depends = depends)
  
}


dsl_parse_expr_assignment_rhs_dim <- function(expr, call) {
  rhs <- expr[[3]]
  
  throw_bad_dim_arg <- function() {
    dsl_parse_error(
      "When using 'dim()' on the right-hand-side, it takes only an array name",
      "E106", expr, call)
  }
  
  throw_invalid_rhs_dim <- function(err) {
    dsl_parse_error(
      "Invalid function{?s} used on rhs of 'dim()': {squote(err)}",
      "E107", expr, call)
  }
  
  if (rlang::is_call(rhs, "c")) {
    value <- as.list(rhs[-1])
  } else {
    value <- list(rhs)
  }
  depends <- join_dependencies(lapply(value, find_dependencies))
  
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


dsl_parse_expr_check_lhs_name <- function(lhs, special, is_array, expr, call) {
  is_stochastic <- identical(special, "~")
  
  if (!rlang::is_symbol(lhs)) {
    ## We will error, the only question is how.
    if (is_array) {
      if (is_stochastic) {
        context <- "on the lhs of a `~` array relationship"
      } else {
        context <- "on the lhs of array assignment"
      } 
    } else {
      if (is_stochastic) {
        context <- "on the lhs of a `~` relationship"
      } else {
        context <- "on the lhs of assignment"
      }
    }
    lhs_str <- deparse1(lhs)
    
    
    if (!rlang::is_call(lhs)) {
      dsl_parse_error("Invalid target '{lhs_str}' {context}",
                       "E102", expr, call)
    }
    
    
    fn_str <- deparse1(lhs[[1]])
    if (is_stochastic || is.null(special)) {
      fn_near <- near_match(fn_str, SPECIAL_LHS)
      if (!is_stochastic && length(fn_near) == 1) {
        hint <- c(i = "Did you mean '{fn_near}()'?")
      } else {
        hint <- NULL
      }
      dsl_parse_error(
        c("Invalid special function '{fn_str}()' {context}",
          hint),
        "E102", expr, call)
    }
  }
  name <- deparse1(lhs)
  
  if (name %in% RESERVED_MONTY) {
    err_in <- "the monty DSL"
    dsl_parse_error(
      c("Can't assign to reserved name '{name}'",
        i = "'{name}' is a reserved word in {err_in}"),
      "E109", expr, call)
  }
  
  if (grepl(RESERVED_MONTY_PREFIX_RE, name)) {
    prefix <- sub(RESERVED_MONTY_PREFIX_RE, "\\1", name)
    dsl_parse_error(
      "Invalid name '{name}' starts with reserved prefix '{prefix}'",
      "E110", expr, call)
  }
  
  if (name == "pi") {
    dsl_parse_error(
      "Do not use `pi` on the left-hand-side of an expression",
      "E111", expr, call)
  }
  
  
  name
}


dsl_parse_check_system <- function(exprs, arrays, fixed, call) {
  special <- vcapply(exprs, function(x) x$special %||% "")
  is_dim <- special == "dim"
  
  exprs <- dsl_parse_check_system_arrays(exprs, arrays, call)
  dsl_parse_check_duplicates(exprs, arrays, is_dim, call)
  
  exprs <- exprs[!is_dim]
  dsl_parse_check_fixed(exprs, fixed, call)
  dsl_parse_check_usage(exprs, fixed, call)
  
  exprs
}


dsl_parse_check_system_arrays <- function(exprs, arrays, call) {
  dim_nms <- arrays$name
  ## First, look for any array calls that do not have a corresponding
  ## dim()
  is_array <- !vlapply(exprs, function(x) is.null(x$lhs$array))
  err <- !vlapply(exprs[is_array], function(x) x$lhs$name %in% dim_nms)
  if (any(err)) {
    err_exprs <- exprs[is_array][err]
    err_nms <- unique(vcapply(err_exprs, function(x) x$lhs$name))
    dsl_parse_error(
      paste("Missing 'dim()' for expression{?s} assigned as an array:",
            "{squote(err_nms)}"),
      "E209", lapply(err_exprs, "[[", "expr"), call)
  }
  
  ## Next, we collect up any subexpressions, in order, for all arrays,
  ## and make sure that we are always assigned as an array.
  nms <- vcapply(exprs, 
                 function(x) if (isTRUE(x$special == "dim")) "" else x$lhs$name)
  for (nm in dim_nms) {
    i <- nms == nm
    err <- vlapply(exprs[i], function(x) is.null(x$lhs$array))
    if (any(err)) {
      err_exprs <- exprs[i][err]
      dsl_parse_error(
        c("Array expressions must always use '[]' on the lhs",
          i = paste("Your expression for '{nm}' has a 'dim()' equation, so it",
                    "is an array, but {cli::qty(sum(err))}",
                    "{?this usage assigns/these usages assign} it as if it were a",
                    "scalar")),
        "E210", lapply(err_exprs, "[[", "expr"), call)
    }
  }
  
  ## now we have checked dim calls
  exprs <- lapply(exprs, dsl_parse_array_index, arrays, call)
  
  exprs
}


dsl_parse_check_duplicates <- function(exprs, arrays, is_dim, call) {
  type <- vcapply(exprs, "[[", "type")
  name <- character(length(exprs))
  name[!is_dim] <- vcapply(exprs[!is_dim], function(x) x$lhs$name)
  
  for (nm in arrays$name) {
    i <- name == nm
    index <- which(i)
    if (any(diff(index) > 1)) {
      index_others <- Filter(
        function(j) j > index[[1]] && j < last(index),
        which(!i))
      others <- unique(vcapply(exprs[index_others], function(x) x$lhs$name))
      e_exprs <- lapply(exprs[seq(index[[1]], last(index))], "[[", "expr")
      dsl_parse_error(
        paste("Multiline array equations must be contiguous",
              "statements, but '{nm}' is interleaved with {squote(others)}"),
        "E211", e_exprs, call)
    }
  }
  
  i_err <- anyDuplicated(name, incomparables = c("", arrays$name))
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

  name <- vcapply(exprs, function(e) e$lhs$name)
  err <- name %in% names(fixed)
  if (any(err)) {
    eq <- exprs[[which(err)[[1]]]]
    dsl_parse_error(
      "Value '{eq$lhs$name}' in 'fixed' is shadowed by {eq$type}",
      "E207", eq$expr, call)
  }
}


dsl_parse_check_usage <- function(exprs, fixed, call) {
  name <- vcapply(exprs, function(x) x$lhs$name)
  names_fixed <- names(fixed)
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    err <- setdiff(e$rhs$depends, c(name[seq_len(i - 1)], names_fixed))
    if (length(err) > 0) {
      ## Out of order:
      out_of_order <- intersect(name, err)
      if (length(out_of_order) > 0) {
        context <- lapply(exprs[name %in% out_of_order], "[[", "expr")
        names(context) <- sprintf("'%s' is defined later:", out_of_order)
        
        ## TODO: It would be nice to indicate that we want to highlight
        ## the variables 'err' here within the expression; that is
        ## probably something rlang can do for us as it does that with
        ## the 'arg' argument to rlang::abort already?
        dsl_parse_error("Invalid use of variable{?s} {squote(err)}",
                        "E205", e$expr, call, context = context)
      } else {
        ## Could also tell the user about variables found in the
        ## calling env, but that requires detecting and then passing
        ## through the correct environment.
        ##
        ## If there is just one unknown variable we will try to find a 
        ## near match
        if (length(err) == 1) {
          nm_near <- near_match(err, c(unique(name), names_fixed))
          if (length(nm_near) == 1) {
            hint <- c(i = "Did you mean '{nm_near}'?")
          } else {
            hint <- NULL
          }
        } else {
          hint <- NULL 
        }
        dsl_parse_error(c("Unknown variable{?s} used: {squote(err)}", hint),
                        "E208", e$expr, call)
      }
    }
  }
}


dsl_packer <- function(parameters, arrays) {
  is_array <- parameters %in% arrays$name
  scalar <- if (all(is_array)) NULL else parameters[!is_array]
  if (any(is_array)) {
    array <- lapply(parameters[is_array], 
                    function(x) unlist(arrays$dims[arrays$name == x]))
    names(array) <- parameters[is_array]
  } else {
    array <- NULL
  }
  monty_packer(scalar, array)
}


dsl_parse_dim_name <- function(name) {
    sprintf("dim_%s", name)
}


dsl_parse_expr_check_lhs_index <- function(name, dim, index, expr, call) {
  ret <- dsl_parse_index(name, dim, index)
  
  if (is.null(ret)) {
    dsl_parse_error(
      "Invalid value for array index lhs",
      "E112", expr, call)
  }
  
  if (any(lengths(ret$depends) > 0)) {
    if (":" %in% ret$depends$functions) {
      dsl_parse_error(
        c("Invalid use of range operator ':' on lhs of array assignment",
          paste("If you use ':' as a range operator on the lhs of an",
                "assignment into an array, then it must be the outermost",
                "call, for e.g, {.code (a + 1):(b + 1)}, not",
                "{.code 1 + (a:b)}")),
        "E113", expr, call)
    }
    allowed <- c("+", "-", "(", ":", "length", "nrow", "ncol")
    err <- setdiff(ret$depends$functions, allowed)
    if (length(err) > 0) {
      dsl_parse_error(
        "Invalid function{?s} used in lhs of array assignment: {squote(err)}",
        "E114", expr, call)
    }
    if ("-" %in% ret$depends$functions && uses_unary_minus(index)) {
      dsl_parse_error(
        "Invalid use of unary minus in lhs of array assignment",
        "E115", expr, call)
    }
    err <- intersect(INDEX, ret$depends$variables)
    if (length(err) > 0) {
      dsl_parse_error(
        paste("Invalid use of special variable{?s} in lhs of array",
              "assignment: {squote(err)}"),
        "E116", expr, call)
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
      dsl_parse_error(
        c("Invalid index access used on rhs of equation: {squote(err)}",
          i = paste("Your lhs has only {n} dimension{?s}, but index '{v}'",
                    "would require {match(v, INDEX)}")),
        "E108", expr, call)
    }
    ## index variables are not real dependencies, so remove them:
    variables <- setdiff(variables, INDEX)
  }
  variables
}
