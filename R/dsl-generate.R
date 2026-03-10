dsl_generate <- function(dat) {
  env <- new.env(parent = asNamespace("monty"))
  env$packer <- dat$packer
  env$fixed <- dat$fixed
  env$parameters <- dat$parameters

  meta <- list(
    pars = quote(pars),
    data = quote(data),
    density = quote(density),
    fixed = quote(fixed),
    fixed_contents = names(env$fixed))

  density <- dsl_generate_density(dat, env, meta)
  direct_sample <- dsl_generate_direct_sample(dat, env, meta)
  gradient <- dsl_generate_gradient(dat, env, meta)
  domain <- dsl_generate_domain(dat, meta, env$packer)
  properties <- monty_model_properties(allow_multiple_parameters = TRUE)
  monty_model(
    list(parameters = env$packer$names(),
         density = density,
         gradient = gradient,
         domain = domain,
         direct_sample = direct_sample),
    properties)
}


dsl_generate_density <- function(dat, env, meta) {
  initial <- dsl_generate_initialise_arrays(dat, "density", meta)
  exprs <- lapply(dat$exprs, dsl_generate_density_expr, meta)
  
  body_exprs <- c(call("<-", meta[["pars"]], quote(packer$unpack(x))),
            call("<-", meta[["density"]], quote(list())),
            initial,
            exprs,
            call("sum", call("unlist", meta[["density"]])))
  if (is.null(dat$domain)) {
    body <- rlang::call2("{", !!!body_exprs)
  } else {
    if (nrow(dat$domain) == 1) {
      domain_min <- dat$domain[[1]]
      domain_max <- dat$domain[[2]]
      in_domain <- bquote(x >= .(domain_min) && x <= .(domain_max))
    } else {
      domain_min <- rlang::call2("c", !!!unname(dat$domain[, 1]))
      domain_max <- rlang::call2("c", !!!unname(dat$domain[, 2]))
      in_domain <- bquote(all(x >= .(domain_min) & x <= .(domain_max)))
    }
    body <- call(
      "{",
      call("if", in_domain, rlang::call2("{", !!!body_exprs), call("{", -Inf)))
  }
  vectorise_density_over_parameters(
    as_function(alist(x = ), body, env))
}


dsl_generate_gradient <- function(dat, env, meta) {
  if (is.null(dat$adjoint)) {
    return(NULL)
  }

  i_main <- match(dat$adjoint$exprs_main, 
                  vcapply(dat$exprs, function(x) x$lhs$name))
  exprs <- c(dat$exprs[i_main], dat$adjoint$exprs)

  eqs <- lapply(exprs, dsl_generate_assignment, "data", meta)
  eq_return <- fold_c(
    lapply(dat$adjoint$gradient, function(nm) call("[[", meta[["data"]], nm)))

  body <- c(call("<-", meta[["data"]], quote(packer$unpack(x))),
            unname(eqs),
            eq_return)
  vectorise_gradient_over_parameters(
    as_function(alist(x = ), body, env),
    length(dat$parameters))
}


dsl_generate_direct_sample <- function(dat, env, meta) {
  if (!is.null(dat$domain)) {
    return(NULL)
  }
  initial <- dsl_generate_initialise_arrays(dat, "pars", meta)
  exprs <- lapply(dat$exprs, dsl_generate_sample_expr, meta)
  body <- c(call("<-", meta[["pars"]], quote(list())),
            initial,
            exprs,
            call("<-", meta[["pars"]], 
                 bquote(.(meta[["pars"]])[parameters])),
            bquote(packer$pack(.(meta[["pars"]]))))
  as_function(alist(rng = ), body, env)
}


dsl_generate_density_expr <- function(expr, meta) {
  switch(expr$type,
         assignment = dsl_generate_assignment(expr, "pars", meta),
         stochastic = dsl_generate_density_stochastic(expr, meta),
         cli::cli_abort(paste(
           "Unimplemented expression type '{expr$type}';",
           "this is a monty bug")))
}


dsl_generate_sample_expr <- function(expr, meta) {
  switch(expr$type,
         assignment = dsl_generate_assignment(expr, "pars", meta),
         stochastic = dsl_generate_sample_stochastic(expr, meta),
         cli::cli_abort(paste(
           "Unimplemented expression type '{expr$type}';",
           "this is a monty bug")))
}


dsl_generate_assignment <- function(expr, dest, meta) {
  array <- expr$lhs$array
  lhs <- bquote(.(meta[[dest]])[[.(expr$lhs$name)]])
  if (!is.null(array)) {
    idx <- lapply(array, function(x) as.name(x$name))
    lhs <- rlang::call2("[", lhs, !!!idx)
  }
  rhs <- expr$rhs$expr
  expr <- rlang::call2("<-", lhs,
                       dsl_generate_density_rewrite_lookup(rhs, dest, meta))

  
  if (!is.null(array)) {
    expr <- dsl_generate_array_loops(expr, array)
  }
  expr
}


dsl_generate_density_stochastic <- function(expr, meta) {
  array <- expr$lhs$array
  name <- as.name(expr$lhs$name)
  lhs <- bquote(.(meta[["density"]])[[.(expr$lhs$name)]])
  if (!is.null(array)) {
    idx <- lapply(array, function(x) as.name(x$name))
    lhs <- rlang::call2("[", lhs, !!!idx)
    name <- rlang::call2("[", name, !!!idx)
  }
  rhs <- rlang::call2(expr$rhs$distribution$density,
                      name, !!!expr$rhs$distribution$args)
  expr <- rlang::call2("<-", lhs,
                       dsl_generate_density_rewrite_lookup(rhs, "pars", meta))
    
  if (!is.null(array)) {
    expr <- dsl_generate_array_loops(expr, array)
  }
  expr
}


dsl_generate_sample_stochastic <- function(expr, meta) {
  array <- expr$lhs$array
  lhs <- bquote(.(meta[["pars"]])[[.(expr$lhs$name)]])
  if (!is.null(array)) {
    idx <- lapply(array, function(x) as.name(x$name))
    lhs <- rlang::call2("[", lhs, !!!idx)
  }
  args <- lapply(expr$rhs$distribution$args, 
                 dsl_generate_density_rewrite_lookup, "pars", meta)
  rhs <- rlang::call2(expr$rhs$distribution$sample, !!!args, quote(rng))
  expr <- rlang::call2("<-", lhs, rhs)
  
  if (!is.null(array)) {
    expr <- dsl_generate_array_loops(expr, array)
  }
  expr
}


dsl_generate_density_rewrite_lookup <- function(expr, dest, meta) {
  if (is.recursive(expr)) {
    expr[-1] <- lapply(expr[-1], dsl_generate_density_rewrite_lookup,
                       dest, meta)
    as.call(expr)
  } else if (is.name(expr)) {
    if (deparse(expr) %in% INDEX) {
      return(expr)
    }
    if (as.character(expr) %in% meta$fixed_contents) {
      dest <- meta$fixed
    }
    call("[[", meta[[as.character(dest)]], as.character(expr))
  } else {
    expr
  }
}


dsl_generate_array_loops <- function(expr, array) {
  for (x in rev(array)) {
    if (x$type == "range") {
      range <- call("seq", x$from, x$to)
      expr <- call("for", as.symbol(x$name), range, call("{", expr))
    } else { # type is "single"
      idx <- list()
      idx[[x$name]] <- x$at
      expr <- substitute_(expr, idx)
    }
  }
  expr
}


dsl_generate_initialise_arrays <- function(dat, dest, meta) {
  parameters <- dat$parameters[dat$parameters %in% dat$arrays$name]
  assigned <- dat$assigned[dat$assigned %in% dat$arrays$name]
  arrays <- dat$arrays
  
  c(lapply(assigned, dsl_generate_initialise_arrays_expr, arrays, "pars", meta),
    lapply(parameters, dsl_generate_initialise_arrays_expr, arrays, dest, meta))
}

dsl_generate_initialise_arrays_expr <- function(name, arrays, dest, meta) {
  dims <- arrays$dims[arrays$name == name][[1]]
  
  lhs <- bquote(.(meta[[as.character(dest)]])[[.(name)]])
  
  if (length(dims) == 1) {
    expr <- call("<-", lhs, call("numeric", dims[[1]]))
  } else {
    expr <- call("<-", lhs, call("array", 0, rlang::call2("c", !!!dims)))
  }
  expr
}


dsl_generate_domain <- function(dat, meta, packer) {
  n <- length(packer$names())
  domain <- cbind(rep(-Inf, n), rep(Inf, n))
  rownames(domain) <- packer$names()
  if (is.null(dat$fixed)) {
    env <- new.env(parent = baseenv())
  } else {
    env <- list2env(dat$fixed, parent = baseenv())
  }
  for (e in dat$exprs) {
    if (e$type == "assignment") {
      dsl_generate_domain_assignment(e, env, dat$arrays)
    } else { # type is "stochastic"
      domain <- dsl_generate_domain_stochastic(domain, e, env)
    }
  }
  
  ## Same logic as model_combine_domain
  if (!is.null(dat$domain)) {
    domain[, 1] <- pmax(domain[, 1], dat$domain[, 1])
    domain[, 2] <- pmin(domain[, 2], dat$domain[, 2])
  }

  domain
}


dsl_generate_domain_assignment <- function(expr, env, arrays) {
  if (is.null(expr$lhs$array)) {
    env[[expr$lhs$name]] <- dsl_static_eval(expr$rhs$expr, env) 
  } else {
    name <- expr$lhs$name
    if (is.null(env[[name]])) {
      dims <- unlist(arrays$dims[arrays$name == name])
      env[[name]] <- array(NA_real_, dims)
    }
    array <- expr$lhs$array
    
    idx <- list()
    for (x in array) {
      from <- x$from %||% x$at
      to <- x$to  %||% x$at
      idx[[x$name]] <- seq(from = from, to = to)
    }
    idx <- expand.grid(rev(idx))
    idx <- idx[, rev(names(idx)), drop = FALSE]
    for (i in seq_len(nrow(idx))) {
      rhs <- substitute_(expr$rhs$expr, as.list(idx[i, , drop = FALSE]))
      rhs_val <- dsl_static_eval(rhs, env)
      env[[name]] <- do.call(`[<-`, c(list(env[[name]]), idx[i, ], rhs_val))
    }
  }
}


dsl_generate_domain_stochastic <- function(domain, expr, env) {
  distr <- expr$rhs$distribution
  
  if (is.null(expr$lhs$array)) {
    domain[expr$lhs$name, ] <- dsl_domain_eval(distr$domain, distr$args, env)
  } else {
    array <- expr$lhs$array
    
    idx <- list()
    for (x in array) {
      from <- x$from %||% x$at
      to <- x$to  %||% x$at
      idx[[x$name]] <- seq(from = from, to = to)
    }
    idx <- expand.grid(rev(idx))
    idx <- idx[, rev(names(idx)), drop = FALSE]
    for (i in seq_len(nrow(idx))) {
      args <- 
        lapply(distr$args, 
               function(x) substitute_(x, as.list(idx[i, , drop = FALSE])))
      name_i <- paste0(expr$lhs$name, "[", paste(idx[i, ], collapse = ","), "]")
      domain[name_i, ] <- dsl_domain_eval(distr$domain, args, env)
    }
  }
  
  domain
}


dsl_static_eval <- function(expr, env) {
  uses <- all.vars(expr)
  if (length(uses) == 0 || all(uses %in% names(env))) {
    eval(expr, env)
  } else {
    NA_real_
  }
}


dsl_domain_eval <- function(domain, args, env) {
  if (is.function(domain)) {
    args <- lapply(args, dsl_static_eval, env)
    do.call(domain, args)
  } else {
    domain
  }
}


fold_c <- function(x) {
  if (length(x) == 1) x[[1]] else as.call(c(quote(c), x))
}


## We can actually do much better than this, but it feels best to wait
## until the rest of the DSL is written, especially arrays.  For
## simple models with scalars we should be able to just pass through
## multiple parameters at once.
vectorise_density_over_parameters <- function(density) {
  function(x) {
    if (is.matrix(x)) {
      vnapply(seq_len(ncol(x)), function(i) density(x[, i]))
    } else {
      density(x)
    }
  }
}

vectorise_gradient_over_parameters <- function(gradient, len) {
  function(x) {
    if (is.matrix(x)) {
      g <- vapply(seq_len(ncol(x)), function(i) gradient(x[, i]), numeric(len))
      if (is.null(dim(g))) {
        dim(g) <- c(len, ncol(x))
      }
      g
    } else {
      gradient(x)
    }
  }
}
