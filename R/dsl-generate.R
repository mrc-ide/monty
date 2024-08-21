dsl_generate <- function(dat) {
  env <- new.env(parent = asNamespace("monty"))
  env$packer <- monty_packer(dat$parameters)

  density <- dsl_generate_density(dat, env)
  direct_sample <- dsl_generate_direct_sample(dat, env)
  gradient <- dsl_generate_gradient(dat, env)
  domain <- dsl_generate_domain(dat)
  monty_model(
    list(parameters = dat$parameters,
         density = density,
         gradient = gradient,
         domain = domain,
         direct_sample = direct_sample))
}


dsl_generate_density <- function(dat, env) {
  exprs <- lapply(dat$exprs, dsl_generate_density_expr,
                  quote(pars), quote(density))
  body <- c(quote(pars <- packer$unpack(x)),
            quote(density <- numeric()),
            exprs,
            quote(sum(density)))
  as_function(alist(x = ), body, env)
}


dsl_generate_gradient <- function(dat, env) {
  if (is.null(dat$adjoint)) {
    return(NULL)
  }

  i_main <- match(dat$adjoint$exprs_main, vcapply(dat$exprs, "[[", "name"))
  exprs <- c(dat$exprs[i_main], dat$adjoint$exprs)

  eqs <- lapply(exprs, dsl_generate_assignment, quote(data))
  eq_return <- fold_c(
    lapply(dat$adjoint$gradient, function(nm) call("[[", quote(data), nm)))

  body <- c(quote(data <- packer$unpack(x)),
            unname(eqs),
            eq_return)
  as_function(alist(x = ), body, env)
}


dsl_generate_direct_sample <- function(dat, env) {
  exprs <- lapply(dat$exprs, dsl_generate_sample_expr,
                  quote(pars), quote(result))
  body <- c(quote(pars <- list()),
            exprs,
            quote(unlist(pars[packer$parameters], FALSE, FALSE)))
  as_function(alist(rng = ), body, env)
}


dsl_generate_density_expr <- function(expr, env, density) {
  switch(expr$type,
         assignment = dsl_generate_assignment(expr, env),
         stochastic = dsl_generate_density_stochastic(expr, env, density),
         cli::cli_abort(paste(
           "Unimplemented expression type '{expr$type}';",
           "this is a monty bug")))
}


dsl_generate_sample_expr <- function(expr, env, result) {
  switch(expr$type,
         assignment = dsl_generate_assignment(expr, env),
         stochastic = dsl_generate_sample_stochastic(expr, env),
         cli::cli_abort(paste(
           "Unimplemented expression type '{expr$type}';",
           "this is a monty bug")))
}


dsl_generate_assignment <- function(expr, env) {
  e <- expr$expr
  e[[2]] <- call("[[", env, as.character(e[[2]]))
  e[[3]] <- dsl_generate_density_rewrite_lookup(e[[3]], env)
  e
}


dsl_generate_density_stochastic <- function(expr, env, density) {
  lhs <- bquote(.(density)[[.(expr$name)]])
  rhs <- rlang::call2(expr$distribution$density,
                      as.name(expr$name), !!!expr$distribution$args)
  rlang::call2("<-", lhs, dsl_generate_density_rewrite_lookup(rhs, env))
}


dsl_generate_sample_stochastic <- function(expr, env) {
  lhs <- bquote(.(env)[[.(expr$name)]])
  args <- lapply(expr$distribution$args, dsl_generate_density_rewrite_lookup,
                 env)
  rhs <- rlang::call2(expr$distribution$sample, quote(rng), !!!args)
  rlang::call2("<-", lhs, rhs)
}


dsl_generate_density_rewrite_lookup <- function(expr, env) {
  if (is.recursive(expr)) {
    expr[-1] <- lapply(expr[-1], dsl_generate_density_rewrite_lookup, env)
    as.call(expr)
  } else if (is.name(expr)) {
    call("[[", env, as.character(expr))
  } else {
    expr
  }
}


dsl_generate_domain <- function(dat) {
  n <- length(dat$parameters)
  domain <- cbind(rep(-Inf, n), rep(Inf, n))
  rownames(domain) <- dat$parameters
  env <- new.env(parent = baseenv())
  for (e in dat$exprs) {
    if (e$type == "assignment") {
      env[[e$name]] <- dsl_static_eval(e$rhs, env)
    } else { # type is "stochastic"
      e_domain <- e$distribution$domain
      if (is.function(e_domain)) {
        args <- lapply(e$distribution$args, dsl_static_eval, env)
        domain[e$name, ] <- do.call(e_domain, args)
      } else {
        domain[e$name, ] <- e_domain
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


fold_c <- function(x) {
  if (length(x) == 1) x[[1]] else as.call(c(quote(c), x))
}
