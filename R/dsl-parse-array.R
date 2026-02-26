dsl_parse_arrays <- function(exprs, fixed, call) {
  special <- vcapply(exprs, function(x) x$special %||% "")
  is_dim <- special == "dim"
  
  arrays <- build_array_table(exprs[is_dim], call)
  check_duplicate_dims(arrays, exprs, call)
  arrays <- resolve_array_references(arrays)
  arrays <- resolve_split_dependencies(arrays, call)
  arrays <- finalise_array_table(arrays, fixed, call)
  
  exprs <- lapply(exprs[!is_dim], dsl_parse_expand_arrays, arrays, call)
  
  exprs <- unlist(exprs, recursive = FALSE)
  
  list(exprs = exprs,
       arrays = arrays)
}


dsl_parse_expand_arrays <- function(expr, arrays, call) {
  if (is.null(expr$lhs$array)) {
    ret <- list(type = expr$type,
                name = expr$lhs$name,
                depends = expr$rhs$depends,
                expr = expr$expr)
    if (expr$type == "stochastic") {
      ret$distribution <- expr$rhs$distribution
    } else {
      ret$rhs <- expr$rhs$expr
    }
    return(list(ret))
  }
  
  idx <- list()
  for (x in expr$lhs$array) {
    nm <- x$name
    if (x$type == "single") {
      from <- x$at
      to <- x$at
    } else {
      from <- x$from
      to <- x$to
    }
    if (rlang::is_call(to, "dsl_dim")) {
      to <- arrays$dims[[which(arrays$name == to[[2]])]][[to[[3]]]]
    }
    idx[[nm]] <- seq(from = from, to = to, by = 1)
  }
  idx <- idx[match(names(idx), INDEX)]
  idx <- expand.grid(rev(idx))
  idx <- idx[, rev(names(idx)), drop = FALSE]
  idx <- split(idx, seq_len(nrow(idx)))
  
  lapply(idx, function(x) generate_array_loop(expr, x))
  
}

generate_array_loop <- function(expr, idx, call) {
  type <- expr$type
  name <- paste(expr$lhs$name, "[", paste(idx, collapse = ", "), "]", sep = "")
  
  rhs_expr <- generate_array_loop_rhs(expr$rhs$expr, idx)
  
  depends <- generate_array_loop_depends(rhs_expr, expr$rhs$depends)
  
  ret <- list(type = expr$type,
              name = name,
              depends = depends,
              expr = expr$expr)
  
  if (type == "assignment") {
    ret$rhs <- rhs_expr
  } else {
    ret$distribution <- expr$rhs$distribution
    ret$distribution$args <- 
      lapply(ret$distribution$args, generate_array_loop_rhs, idx)
  }
  
  ret
}


generate_array_loop_rhs <- function(rhs, idx) {
  rhs <- insert_index_rhs(rhs, idx)
  eval_index_rhs(rhs)
}


insert_index_rhs <- function(rhs, idx) {
  if (length(rhs) == 1) {
    if (as.character(rhs) %in% names(idx)) {
      rhs <- idx[[which(rhs == names(idx))]]
    }
  } else {
    for (i in seq_len(length(rhs))) {
      rhs[[i]] <- insert_index_rhs(rhs[[i]], idx)
    }
  }
  rhs
}


eval_index_rhs <- function(rhs) {
  if (length(rhs) > 1) {
    if (rhs[[1]] == "[") {
      rhs[3:length(rhs)] <- lapply(rhs[3:length(rhs)], eval)
    } else {
      for (i in seq_len(length(rhs))) {
        rhs[[i]] <- eval_index_rhs(rhs[[i]])
      }
    }
  }
  rhs
}


generate_array_loop_depends <- function(expr, depends) {
  if (length(expr) > 1) {
    if (expr[[1]] == "[") {
      depends <- depends[depends != expr[[2]]]
      depends <- c(depends, deparse(expr))
    } else {
      for (i in seq_len(length(expr))) {
        depends <- generate_array_loop_depends(expr[[i]], depends)  
      }
    } 
  }
  depends
}


build_array_table <- function(exprs, call) {
  dims <- list()
  names <- list()
  n <- 1
  for (expr in exprs) {
    names_i <- expr$lhs$names
    dims_i <- expr$rhs$value
    
    first_dim <- call("dim", as.symbol(expr$lhs$names[1]))
    
    for (j in seq_along(names_i)) {
      dims[[n]] <- if (j == 1) dims_i else first_dim
      names[[n]] <- names_i[j]
      n <- n + 1
    }
  }
  
  data_frame(
    name = unlist(names),
    rank = lengths(dims),
    dims = I(dims))
}


check_duplicate_dims <- function(arrays, exprs, call) {
  throw_duplicate_dim <- function(name, src) {
    odin_parse_error(
      paste("The variable {name} was given dimensions multiple times."),
      "E2021", src, call)
  }
  
  names <- unlist(arrays$name)
  if (any(duplicated(names))) {
    dup_dim <- unique(names[duplicated(names)])[1]
    lines <- vlapply(exprs, function(x) {
      isTRUE(x$special == "dim" &
               dup_dim %in% c(x$lhs$names))
    })
    srcs <- lapply(exprs[lines], "[[", "src")
    throw_duplicate_dim(dup_dim, srcs)
  }
}


resolve_array_references <- function(arrays) {
  lookup_array <- function(name, copy_from, d) {
    i <- which(d$name == copy_from)
    if (length(i) == 0) {
      return(NULL)
    }
    dim_i <- d$dims[i]
    if (rlang::is_call(dim_i[[1]], "dim")) {
      rhs_dim_var <- deparse(dim_i[[1]][[2]])
      return(lookup_array(name, rhs_dim_var, d[-i, ]))
    }
    return(list(rank = d$rank[i], alias = d$name[i]))
  }
  
  arrays$alias <- arrays$name
  is_ref <- vlapply(arrays$dims, rlang::is_call, "dim")
  
  for (i in which(is_ref)) {
    lhs_dim_var <- arrays$name[i]
    rhs_dim_var <- deparse(arrays$dims[i][[1]][[2]])
    res <- lookup_array(lhs_dim_var, rhs_dim_var, arrays[-i, ])
    if (!is.null(res)) {
      arrays$dims[i] <- list(NULL)
      arrays$rank[i] <- res$rank
      arrays$alias[i] <- res$alias
    }
  }
  
  arrays
}

resolve_split_dependencies <- function(arrays, call) {
  # Resolve case where
  #   dim(a) <- 1
  #   dim(b, c) <- dim(a)
  # At this point, dim(c) will be aliased to dim(b), not dim(a),
  # so find aliases that actually point to other aliases, and
  # resolve them to something that is not an alias.
  
  find_non_alias <- function(current, original = current, visited = NULL) {
    stopifnot(!any(duplicated(visited)))
    
    array <- arrays[arrays$name == current, ]
    if (array$alias == array$name) {
      return(array$alias)
    }
    find_non_alias(array$alias, c(visited, original, array$name))
  }
  
  not_aliased <- arrays$name[arrays$name == arrays$alias]
  wrong <- arrays$name != arrays$alias & !(arrays$alias %in% not_aliased)
  for (i in which(wrong)) {
    arrays$alias[i] <- find_non_alias(arrays$alias[i], arrays$name[i])
  }
  
  arrays
}


finalise_array_table <- function(arrays, fixed, call) {
  evaluate_symbol_dims <- function(d) {
    if (is.symbol(d)) {
      ## TODO: add error for when not in fixed
      d <- fixed[[deparse1(d)]]
    }
    d
  }
  
  arrays$dims <- 
    lapply(arrays$dims, function(x) lapply(x, evaluate_symbol_dims))
  
  is_alias <- arrays$alias != arrays$name
  arrays$dims[is_alias] <- 
    arrays$dims[match(arrays$alias[is_alias], arrays$name)]
  
  arrays
}
