dsl_parse_adjoint <- function(parameters, exprs, required, call = NULL) {
  if (isFALSE(required)) {
    return(NULL)
  }
  rlang::try_fetch({
    exprs <- adjoint_rewrite_stochastic(parameters, exprs)
    adjoint_create(parameters, exprs)
  },
  monty_parse_error = function(e) {
    if (is.null(required)) {
      ## TODO: this might change as #52 is merged, to print slightly
      ## more nicely.  At present we need one extra parent on this
      ## than ideal because otherwise we don't get the contextual
      ## information about the differentiation failure attached to
      ## the warning.
      cli::cli_warn(
        c("Not creating a gradient function for this model",
          i = paste("Pass 'gradient = FALSE' to disable creating the",
                    "gradient function, which will disable this warning")),
        parent = e)
      NULL
    } else {
      rlang::zap() # not handling this, throw it anyway.
    }
  })
}


## The first step is to rewrite our equations of the form
##
## > a ~ Normal(mu, sd)
##
## as
##
## > __density_a <- -(x - mean)^2/(2 * sd^2) - log(2 * pi)/2 - log(sd)
##
## which makes them available for differentiation.  We do this by
## replacing the stochastic equations with new assignment equations,
## given some prefix
##
## We also need a root equation which computes the sum of all
## densities.
##
## > __density_a + __density_b + ... + __density_n
adjoint_rewrite_stochastic <- function(parameters, exprs, call = NULL) {
  prefix_density <- "__density"
  f <- function(eq) {
    if (eq$type == "assignment") {
      eq
    } else {
      lhs <- eq$lhs
      lhs$name <- paste0(prefix_density, lhs$name)
      args <- set_names(c(as.name(eq$lhs$name), eq$rhs$distribution$args),
                        names(formals(eq$rhs$distribution$density)))
      density_expr <- eq$rhs$distribution$expr$density
      if (is.null(density_expr)) {
        dsl_parse_error(
          "Density for '{eq$rhs$distribution$name}' not differentiable",
          "E206", eq$expr, call)
      }
      rhs <- 
        list(expr = maths$rewrite(substitute_(density_expr, list2env(args))),
             depends = eq$rhs$depends)
      
      list(type = "assignment",
           special = NULL,
           lhs = lhs,
           rhs = rhs,
           expr = call("<-", as.name(lhs$name), rhs$expr),
           original = eq)
    }
  }

  parts <- paste0(prefix_density, parameters)
  lhs_root <- list(name = "",
                   array = NULL,
                   depends = NULL)
  rhs_root <- list(expr = maths$plus_fold(lapply(parts, as.name)),
                   depends = parts)
  root <- list(type = "root",
               special = NULL,
               lhs = lhs_root,
               depends = parts,
               rhs = rhs_root)
  exprs <- lapply(exprs, f)
  names(exprs) <- vcapply(exprs, function(x) x$lhs$name)
  c(exprs, list(root))
}


## This step builds the adjoint system by working backwards through
## the set of equations.
adjoint_create <- function(parameters, exprs, call = NULL) {
  prefix_adjoint <- "__adjoint_"
  f <- function(e) {
    if (nzchar(e$lhs$name)) {
      c(e$lhs$name, e$rhs$depends)
    } else {
      e$rhs$depends
    }
  } 
  deps <- lapply(exprs, f)

  ## Adjoint expressions will be collected here:
  adj <- list()

  ## Helper to stop us retaining "__adjoint_x" variables where they
  ## have constant numeric values.
  lookup_or_value <- function(nm) {
    value <- adj[[nm]]$rhs$expr
    if (is.numeric(value)) value else as.name(nm)
  }

  ## Helper to make the loop below easier to understand.  This helps
  ## by throwing an error that contains information about the source
  ## line that caused the differentiation failure, rather than just
  ## the generic information about which function failed to be
  ## differentiated.
  differentiate_or_rethrow <- function(eq, nm) {
    rlang::try_fetch(
      differentiate(eq$rhs$expr, nm),
      monty_differentiation_failure = function(e) {
        expr <- (eq$original %||% eq)$expr
        dsl_parse_error("Failed to differentiate this model",
                        "E206", expr, call = call, parent = e)
      })
  }

  ## We accumulate adjoint expressions and values in adj and look them
  ## up as we go, so this is a sequential loop rather than something
  ## we can lapply over
  for (nm in c(rev(names(exprs)), parameters)) {
    if (nzchar(nm)) {
      i <- vlapply(deps, function(x) any(nm %in% x))
      parts <- lapply(exprs[i], function(eq) {
        maths$times(lookup_or_value(paste0(prefix_adjoint, eq$lhs$name)),
                    differentiate_or_rethrow(eq, nm))
      })
      rhs <- maths$plus_fold(parts)
    } else {
      rhs <- 1
    }
    name <- paste0(prefix_adjoint, nm)
    adj[[name]] <- list(type = "assignment",
                        lhs = list(name = name,
                                   array = NULL),
                        rhs = list(expr = rhs,
                                   depends = all.vars(rhs)),
                        expr = call("<-", as.name(name), rhs))
  }

  ## At this point we have a set of equations where we are interested
  ## in the last few (the adjoints of the parameters)
  nms_gradient <- names(adj)[seq_along(parameters) + length(exprs)]

  ## Go through and work out all the dependencies for this set by
  ## working back down the adjoint equations:
  keep <- nms_gradient
  for (i in rev(names(adj))) {
    if (i %in% keep) {
      keep <- union(adj[[i]]$depends, keep)
    }
  }

  ## The names here are not great, and are subject to change, but
  ## 'exprs_main' is the *names* of the equations in the main model,
  ## while 'exprs' are the values of the expressions in the adjoint
  ## model.
  nms_main <- intersect(keep, names(exprs))
  nms_adj <- intersect(keep, names(adj))

  list(exprs_main = nms_main,
       exprs = adj[nms_adj],
       gradient = nms_gradient)
}
