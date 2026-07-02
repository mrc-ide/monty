dsl_generate <- function(dat) {
  env <- new.env(parent = asNamespace("monty"))
  env$packer <- dat$packer
  env$fixed <- dat$fixed
  env$parameters <- dat$parameters
  env$groups <- dat$group_data$groups
  
  has_parameter_groups <- !is.null(env$groups)
  parameter_groups <- 
    if (has_parameter_groups) env$packer$parameter_groups() else NULL
  
  meta <- list(
    pars = quote(pars),
    data = quote(data),
    density = quote(density),
    fixed = quote(fixed),
    fixed_contents = dsl_fixed_contents(env$fixed, dat$group_data))

  density <- dsl_generate_density(dat, env, meta)
  direct_sample <- dsl_generate_direct_sample(dat, env, meta)
  gradient <- dsl_generate_gradient(dat, env, meta)
  domain <- dsl_generate_domain(dat, meta, env$packer)
  
  properties <- 
    monty_model_properties(allow_multiple_parameters = TRUE,
                           has_parameter_groups = has_parameter_groups)
  monty_model(
    list(parameters = env$packer$names(),
         parameter_groups = parameter_groups,
         groups = env$groups,
         density = density,
         gradient = gradient,
         domain = domain,
         direct_sample = direct_sample),
    properties)
}


dsl_generate_density <- function(dat, env, meta) {
  initial <- dsl_generate_initialise_arrays(dat, "density", meta)
  exprs <- lapply(dat$exprs, dsl_generate_density_expr, dat$group_data, meta)
  if (is.null(env$groups)) {
    tidy <- call("sum", call("unlist", meta[["density"]]))
  } else {
    density_groups <- 
    tidy <- 
      c(call("<-", quote(density_groups), 
             call("[", meta[["density"]], quote(groups))),
        call("<-", quote(density_groups),
             call("lapply", call("lapply", quote(density_groups),
                       quote(unlist)), quote(sum))),
        call("<-", quote(density_groups),
              call("unlist", quote(density_groups))),
        call("<-", meta[["density"]], 
             call("sum", call("unlist", meta[["density"]]))),
        call("<-", call("attr", meta[["density"]], "shared"), 
             meta[["density"]]),
        call("<-", call("attr", meta[["density"]], "groups"), 
             quote(density_groups)),
        call("<-", meta[["density"]],
             call("+", meta[["density"]],
                  call("sum", quote(density_groups)))),
        meta[["density"]])
  }
  
  body_exprs <- unlist(c(call("<-", meta[["pars"]], quote(packer$unpack(x))),
                         call("<-", meta[["density"]], quote(list())),
                         initial,
                         exprs,
                         tidy))
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

  eqs <- lapply(exprs, dsl_generate_assignment, dat$group_data, "data", meta)
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
  exprs <- lapply(dat$exprs, dsl_generate_sample_expr, dat$group_data, meta)
  if (is.null(dat$group_data$groups)) {
    tidy <- call("<-", meta[["pars"]], bquote(.(meta[["pars"]])[parameters]))
  } else {
    tidy <- call("<-", bquote(.(meta[["pars"]])[[group]]), 
                 bquote(.(meta[["pars"]])[[group]][parameters]))
    tidy <- dsl_generate_group_loops(tidy, dat$groups, meta)
  }
  
  body <- unlist(c(call("<-", meta[["pars"]], quote(list())),
                   initial,
                   exprs,
                   tidy,
                   bquote(packer$pack(.(meta[["pars"]])))))
  as_function(alist(rng = ), body, env)
}


dsl_generate_density_expr <- function(expr, group_data, meta) {
  switch(expr$type,
         assignment = dsl_generate_assignment(expr, group_data, "pars", meta),
         stochastic = dsl_generate_density_stochastic(expr, group_data, meta),
         cli::cli_abort(paste(
           "Unimplemented expression type '{expr$type}';",
           "this is a monty bug")))
}


dsl_generate_sample_expr <- function(expr, group_data, meta) {
  switch(expr$type,
         assignment = dsl_generate_assignment(expr, group_data, "pars", meta),
         stochastic = dsl_generate_sample_stochastic(expr, group_data, meta),
         cli::cli_abort(paste(
           "Unimplemented expression type '{expr$type}';",
           "this is a monty bug")))
}


dsl_generate_assignment <- function(expr, group_data, dest, meta) {
  array <- expr$lhs$array
  has_groups <- !is.null(group_data$groups)
  
  if (has_groups) {
    lhs <- bquote(.(meta[[dest]])[[group]][[.(expr$lhs$name)]])
  } else {
    lhs <- bquote(.(meta[[dest]])[[.(expr$lhs$name)]])
  }
  if (!is.null(array)) {
    idx <- lapply(array, function(x) as.name(x$name))
    lhs <- rlang::call2("[", lhs, !!!idx)
  }
  rhs <- expr$rhs$expr
  expr <- rlang::call2("<-", lhs,
                       dsl_generate_density_rewrite_lookup(rhs, dest, meta,
                                                           has_groups))

  
  if (!is.null(array)) {
    expr <- dsl_generate_array_loops(expr, array, meta)
  }
  
  if (has_groups) {
    expr <- dsl_generate_group_loops(expr, group_data$groups, meta)
  }
  
  expr
}


dsl_generate_density_stochastic <- function(expr, group_data, meta) {
  array <- expr$lhs$array
  name <- as.name(expr$lhs$name)
  is_grouped <- !is.null(expr$lhs$group)
  has_groups <- !is.null(group_data$groups)
  
  ## for grouped parameters we need to evaluate the density for each group,
  ## while for shared parameters we need to evaluate the density once
  if (is_grouped) {
    lhs <- bquote(.(meta[["density"]])[[group]][[.(expr$lhs$name)]])
  } else {
    lhs <- bquote(.(meta[["density"]])[[.(expr$lhs$name)]])
  }
  
  if (!is.null(array)) {
    idx <- lapply(array, function(x) as.name(x$name))
    lhs <- rlang::call2("[", lhs, !!!idx)
    name <- rlang::call2("[", name, !!!idx)
  }
  rhs <- rlang::call2(expr$rhs$distribution$density,
                      name, !!!expr$rhs$distribution$args)
  expr <- rlang::call2("<-", lhs,
                       dsl_generate_density_rewrite_lookup(rhs, "pars", meta,
                                                           has_groups))
    
  if (!is.null(array)) {
    expr <- dsl_generate_array_loops(expr, array, meta)
  }
  if (has_groups) {
    if (is_grouped) {
      expr <- dsl_generate_group_loops(expr, group_data$groups, meta)
    } else {
      ## for shared parameters we evaluate the density once - parameters
      ## are unpacked into a list for each group and we just use the value
      ## of the shared parameter from group 1 (will be the same in each group)
      expr <- dsl_generate_group_loops(expr, group_data$groups[1], meta)
    }
  }
  
  
  expr
}


dsl_generate_sample_stochastic <- function(expr, group_data, meta) {
  array <- expr$lhs$array
  is_grouped <- !is.null(expr$lhs$group)
  has_groups <- !is.null(group_data$groups)
  
  if (has_groups) {
    lhs <- bquote(.(meta[["pars"]])[[group]][[.(expr$lhs$name)]])
  } else {
    lhs <- bquote(.(meta[["pars"]])[[.(expr$lhs$name)]])
  }
  
  if (!is.null(array)) {
    idx <- lapply(array, function(x) as.name(x$name))
    lhs <- rlang::call2("[", lhs, !!!idx)
  }
  args <- lapply(expr$rhs$distribution$args, 
                 dsl_generate_density_rewrite_lookup, "pars", meta, has_groups)
  rhs <- rlang::call2(expr$rhs$distribution$sample, !!!args, quote(rng))
  expr <- rlang::call2("<-", lhs, rhs)
  
  if (has_groups) {
    groups <- group_data$groups
    if (is_grouped) {
      expr <- dsl_generate_group_loops(expr, groups, meta)
    } else {
      ## shared parameter so we need to sample once and then assign in the rest
      ## of the groups
      expr2 <- call("<-", lhs, substitute_(lhs, list(group = groups[1])))
      expr <- unlist(c(dsl_generate_group_loops(expr, groups[1], meta),
                       dsl_generate_group_loops(expr2, groups[-1], meta)))
    }
  }
  
  if (!is.null(array)) {
    if (has_groups) {
      expr <- lapply(expr, dsl_generate_array_loops, array, meta)
    } else {
      expr <- dsl_generate_array_loops(expr, array, meta)
    }
  }
  
  expr
}


dsl_generate_density_rewrite_lookup <- function(expr, dest, meta,
                                                is_grouped = FALSE) {
  if (is.recursive(expr)) {
    expr[-1] <- lapply(expr[-1], dsl_generate_density_rewrite_lookup,
                       dest, meta, is_grouped)
    as.call(expr)
  } else if (is.name(expr)) {
    if (deparse(expr) %in% INDEX) {
      return(expr)
    }
    if (as.character(expr) %in% meta$fixed_contents$names) {
      dest <- meta$fixed
      if (!(as.character(expr) %in% meta$fixed_contents$grouped)) {
        is_grouped <- FALSE
      }
    }
    if (is_grouped) {
      expr <- call("[[", 
                   call("[[", meta[[as.character(dest)]], quote(group)),
                   as.character(expr))
    } else {
      expr <- call("[[", meta[[as.character(dest)]], as.character(expr))
    }
    expr
  } else {
    expr
  }
}


dsl_generate_array_loops <- function(expr, array, meta) {
  for (x in rev(array)) {
    if (x$type == "range") {
      range <- call("seq", x$from, x$to)
      range <- dsl_generate_density_rewrite_lookup(range, "pars", meta)
      expr <- call("for", as.symbol(x$name), range, call("{", expr))
    } else { # type is "single"
      idx <- list()
      idx[[x$name]] <- dsl_generate_density_rewrite_lookup(x$at, "pars", meta)
      expr <- substitute_(expr, idx)
    }
  }
  expr
}


dsl_generate_initialise_arrays <- function(dat, dest, meta) {
  parameters <- dat$parameters[dat$parameters %in% dat$arrays$name]
  assigned <- dat$assigned[dat$assigned %in% dat$arrays$name]
  arrays <- dat$arrays
  
  group_data <- dat$group_data
  
  c(lapply(assigned, dsl_generate_initialise_arrays_expr, 
           arrays, group_data, "pars", meta),
    lapply(parameters, dsl_generate_initialise_arrays_expr,
           arrays, group_data, dest, meta))
}


dsl_generate_initialise_arrays_expr <- function(name, arrays, group_data,
                                                dest, meta) {
  dims <- arrays$dims[arrays$name == name][[1]]
  
  has_groups <- !is.null(group_data$groups)
  is_grouped <- name %in% group_data$pars_grouped
  
  is_initialise_grouped <- has_groups && (is_grouped || dest != "density")
  if (is_initialise_grouped) {
    lhs <- bquote(.(meta[[as.character(dest)]])[[group]][[.(name)]])
  } else {
    lhs <- bquote(.(meta[[as.character(dest)]])[[.(name)]])
  }
  
  expr <- call("<-", lhs, call("array", 0, rlang::call2("c", !!!dims)))
  
  if (is_initialise_grouped) {
    expr <- dsl_generate_group_loops(expr, group_data$groups, meta)
  }
  
  expr
}


dsl_generate_group_loops <- function(expr, groups, meta) {
  generate_group_loop <- function(g) {
    substitute_(expr, list(group = g))
  }
  lapply(groups, generate_group_loop)
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
      dsl_generate_domain_assignment(e, env, dat$fixed, dat$arrays,
                                     dat$group_data$groups)
    } else { # type is "stochastic"
      domain <- dsl_generate_domain_stochastic(domain, e, env, dat$fixed,
                                               dat$group_data$groups)
    }
  }
  
  ## Same logic as model_combine_domain
  if (!is.null(dat$domain)) {
    domain[, 1] <- pmax(domain[, 1], dat$domain[, 1])
    domain[, 2] <- pmin(domain[, 2], dat$domain[, 2])
  }

  domain
}


dsl_generate_domain_assignment <- function(expr, env, fixed, arrays, groups) {
  is_grouped <- !is.null(expr$lhs$group)
  if (is.null(expr$lhs$array)) {
    if (is_grouped) {
      for (g in groups) {
        env_g <- c(mget(setdiff(names(env), groups), env), env[[g]])
        env[[g]][[expr$lhs$name]] <- dsl_static_eval(expr$rhs$expr, env_g) 
      }
    } else {
      env[[expr$lhs$name]] <- dsl_static_eval(expr$rhs$expr, env) 
    }
  } else {
    name <- expr$lhs$name
    array <- expr$lhs$array
    idx <- generate_index_grid(array, fixed)
    if (is_grouped) {
      for (g in groups) {
        if (is.null(env[[g]][[name]])) {
          dims <- unlist(arrays$dims[arrays$name == name])
          env[[g]][[name]] <- array(NA_real_, dims)
        }
      
        for (i in seq_len(nrow(idx))) {
          rhs <- substitute_(expr$rhs$expr, as.list(idx[i, , drop = FALSE]))
          env_g <- c(mget(setdiff(names(env), groups), env), env[[g]])
          rhs_val <- dsl_static_eval(rhs, env_g)
          env[[g]][[name]] <- 
            do.call(`[<-`, c(list(env[[g]][[name]]), idx[i, ], rhs_val))
        }
      }
    } else {
      if (is.null(env[[name]])) {
        dims <- unlist(arrays$dims[arrays$name == name])
        env[[name]] <- array(NA_real_, dims)
      }
      
      for (i in seq_len(nrow(idx))) {
        rhs <- substitute_(expr$rhs$expr, as.list(idx[i, , drop = FALSE]))
        rhs_val <- dsl_static_eval(rhs, env)
        env[[name]] <- do.call(`[<-`, c(list(env[[name]]), idx[i, ], rhs_val))
      }
    }
    
  }
}


dsl_generate_domain_stochastic <- function(domain, expr, env, fixed, groups) {
  distr <- expr$rhs$distribution
  is_grouped <- !is.null(expr$lhs$group)
  
  if (is.null(expr$lhs$array)) {
    if (is_grouped) {
      for (g in groups) {
        name <- paste(expr$lhs$name, "|", g)
        env_g <- c(mget(setdiff(names(env), groups), env), env[[g]])
        domain[name, ] <- dsl_domain_eval(distr$domain, distr$args, env_g)
      }
    } else {
      domain[expr$lhs$name, ] <- dsl_domain_eval(distr$domain, distr$args, env)
    }
  } else {
    array <- expr$lhs$array
    
    idx <- list()
    for (x in array) {
      from <- eval(x$from %||% x$at, fixed)
      to <- eval(x$to  %||% x$at, fixed)
      idx[[x$name]] <- seq(from = from, to = to)
    }
    idx <- expand.grid(rev(idx))
    idx <- idx[, rev(names(idx)), drop = FALSE]
    for (i in seq_len(nrow(idx))) {
      
      args <- 
        lapply(distr$args, 
               function(x) substitute_(x, as.list(idx[i, , drop = FALSE])))
      name_i <- paste0(expr$lhs$name, "[", paste(idx[i, ], collapse = ","), "]")
      if (is_grouped) {
        for (g in groups) {
          name <- paste(name_i, "|", g)
          env_g <- c(mget(setdiff(names(env), groups), env), env[[g]])
          domain[name, ] <- dsl_domain_eval(distr$domain, args, env_g)
        }
      } else {
        domain[name_i, ] <- dsl_domain_eval(distr$domain, args, env)
      }
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


dsl_fixed_contents <- function(fixed, group_data) {
  if (is.null(group_data$groups)) {
    names <- names(fixed)
    grouped <- NULL
  } else {
    shared <- setdiff(names(fixed), group_data$groups)
    grouped <- names(fixed[[group_data$groups[1]]])
    names <- c(shared, grouped)
  }
  
  list(names = names,
       grouped = grouped)
}


fold_c <- function(x) {
  if (length(x) == 1) x[[1]] else as.call(c(quote(c), x))
}


generate_index_grid <- function(array, fixed) {
  idx <- list()
  for (x in array) {
    from <- eval(x$from %||% x$at, fixed)
    to <- eval(x$to  %||% x$at, fixed)
    idx[[x$name]] <- seq(from = from, to = to)
  }
  idx <- expand.grid(rev(idx))
  idx[, rev(names(idx)), drop = FALSE]
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
