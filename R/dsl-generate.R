dsl_generate <- function(dat) {
  env <- new.env(parent = asNamespace("monty"))
  env$packer <- monty_packer(dat$parameters)
  env$fixed <- dat$fixed

  meta <- list(
    pars = quote(pars),
    data = quote(data),
    density = quote(density),
    fixed = quote(fixed),
    fixed_contents = names(env$fixed))

  density <- dsl_generate_density(dat, env, meta)
  direct_sample <- dsl_generate_direct_sample(dat, env, meta)
  gradient <- dsl_generate_gradient(dat, env, meta)
  domain <- dsl_generate_domain(dat, meta)
  properties <- monty_model_properties(allow_multiple_parameters = TRUE)
  monty_model(
    list(parameters = dat$parameters,
         density = density,
         gradient = gradient,
         domain = domain,
         direct_sample = direct_sample),
    properties)
}


dsl_generate_density <- function(dat, env, meta) {
  exprs <- lapply(dat$exprs, dsl_generate_density_expr, meta)
  body <- c(call("<-", meta[["pars"]], quote(packer$unpack(x))),
            call("<-", meta[["density"]], quote(numeric())),
            exprs,
            call("sum", meta[["density"]]))
  vectorise_density_over_parameters(
    as_function(alist(x = ), body, env))
}


dsl_generate_gradient <- function(dat, env, meta) {
  if (is.null(dat$adjoint)) {
    return(NULL)
  }

  i_main <- match(dat$adjoint$exprs_main, vcapply(dat$exprs, "[[", "name"))
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
  exprs <- lapply(dat$exprs, dsl_generate_sample_expr, meta)
  body <- c(call("<-", meta[["pars"]], quote(list())),
            exprs,
            bquote(unlist(.(meta[["pars"]])[packer$names()], FALSE, FALSE)))
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
  e <- expr$expr
  e[[2]] <- call("[[", meta[[dest]], as.character(e[[2]]))
  e[[3]] <- dsl_generate_density_rewrite_lookup(e[[3]], dest, meta)
  e
}


dsl_generate_density_stochastic <- function(expr, meta) {
  lhs <- bquote(.(meta[["density"]])[[.(expr$name)]])
  rhs <- rlang::call2(expr$distribution$density,
                      as.name(expr$name), !!!expr$distribution$args)
  rlang::call2("<-", lhs,
               dsl_generate_density_rewrite_lookup(rhs, "pars", meta))
}


dsl_generate_sample_stochastic <- function(expr, meta) {
  lhs <- bquote(.(meta[["pars"]])[[.(expr$name)]])
  args <- lapply(expr$distribution$args, dsl_generate_density_rewrite_lookup,
                 "pars", meta)
  rhs <- rlang::call2(expr$distribution$sample, !!!args, quote(rng))
  rlang::call2("<-", lhs, rhs)
}


dsl_generate_density_rewrite_lookup <- function(expr, dest, meta) {
  if (is.recursive(expr)) {
    expr[-1] <- lapply(expr[-1], dsl_generate_density_rewrite_lookup,
                       dest, meta)
    as.call(expr)
  } else if (is.name(expr)) {
    if (as.character(expr) %in% meta$fixed_contents) {
      dest <- meta$fixed
    }
    call("[[", meta[[as.character(dest)]], as.character(expr))
  } else {
    expr
  }
}


dsl_generate_domain <- function(dat, meta) {
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
