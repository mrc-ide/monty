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
adjoint_rewrite_stochastic <- function(exprs, parameters, prefix_density) {
  f <- function(eq) {
    if (eq$type == "assignment") {
      eq
    } else {
      args <- set_names(c(as.name(eq$name), eq$distribution$args),
                        names(formals(eq$distribution$density)))
      rhs <- maths$rewrite(
        substitute_(eq$distribution$expr$density, list2env(args)))
      name <- paste0(prefix_density, eq$name)
      list(type = "assignment",
           name = name,
           depends = c(eq$name, eq$depends),
           rhs = rhs,
           expr = call("<-", as.name(name), rhs))
    }
  }

  parts <- paste0(prefix_density, parameters)
  root <- list(type = "root",
               name = "",
               depends = parts,
               rhs = maths$plus_fold(lapply(parts, as.name)))
  exprs <- lapply(exprs, f)
  names(exprs) <- vcapply(exprs, "[[", "name")
  c(exprs, list(root))
}


## This step builds the adjoint system by working backwards through
## the set of equations.  The result will be a list of adjoint
## equations.
adjoint_create <- function(exprs, parameters, prefix_adjoint) {
  ## Collect all the dependencies, we'll go through this list quite
  ## few times
  deps <- lapply(exprs, function(e) e$depends)

  ## Adjoint expressions will be collected here:
  adj <- list()

  ## Helper to stop us retaining "__adjoint_x" variables where they
  ## have constant numeric values.
  lookup_or_value <- function(nm) {
    value <- adj[[nm]]$rhs
    if (is.numeric(value)) value else as.name(nm)
  }

  for (nm in c(rev(names(exprs)), parameters)) {
    if (nzchar(nm)) {
      i <- vlapply(deps, function(x) any(nm %in% x))
      parts <- lapply(exprs[i], function(eq) {
        maths$times(lookup_or_value(paste0(prefix_adjoint, eq$name)),
                    differentiate(eq$rhs, nm))
      })
      rhs <- maths$plus_fold(parts)
    } else {
      rhs <- 1
    }
    name <- paste0(prefix_adjoint, nm)
    adj[[name]] <- list(type = "assignment",
                        name = name,
                        rhs = rhs,
                        expr = call("<-", as.name(name), rhs),
                        depends = all.vars(rhs))
  }

  ## At this point we have a set of equations where we are interested
  ## in the last few (the adjoints of the parameters)
  nms_gradient <- names(adj)[seq_along(parameters) + length(exprs)]

  ## Go through and work out all the dependencies for this set by
  ## working back down the adjoint equations:
  keep <- nms_gradient
  for (i in rev(names(adj))) {
    if (i %in% keep) {
      keep <- union(keep, adj[[i]]$depends)
    }
  }

  list(exprs = c(exprs[intersect(keep, names(exprs))],
                 adj[intersect(keep, names(adj))]),
       gradient = nms_gradient)
}
