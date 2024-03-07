##' Combine two models by multiplication.  We'll need a better name
##' here.  In Bayesian inference we will want to create a model that
##' represents the multiplication of a likelihood and a prior (in log
##' space) and it will be convenient to think about these models
##' separately.  Multiplying probabilities (or adding on a log scale)
##' is common enough that there may be other situations where we want
##' to do this.
##'
##' Here we describe the impact of combining a pair of models
##'
##' * `density`: this is the sum of the log densities from each model
##'
##' * `parameters`: the union of parameters from each model is taken
##'
##' * `domain`: The most restrictive domain is taken for each
##'   parameter.  Parameters that do not appear in one model are
##'   assumed to have infinite domain there.
##'
##' * `gradient`: if *both* models define a gradient, this is the sum
##'   of the gradients.  If either does not define a gradient, the
##'   resulting model will not have gradient support.  Set
##'   `has_gradient = TRUE` within `properties if you want to enforce
##'   that the combination is differentiable.  If the models disagree
##'   in their parameters, parameters that are missing from a model
##'   are assumed (reasonably) to have a zero gradient.
##'
##' * `direct_sample`: this one is hard to do the right thing for.  If
##'   neither model can be directly sampled from that's fine, we
##'   don't directly sample.  If only one model can be sampled from
##'   *and* if it can sample from the union of all parameters then we
##'   take that function (this is the case for a prior model when
##'   combined with a likelihood).  Other cases will be errors, which
##'   can be avoided by setting `has_direct_gradient = FALSE` in
##'   `properties`.
##'
##' The properties of the model will be combined as above, reflecting
##' the properties of the joint model.
##'
##' The `model` field will be an ordered, unnamed, list containing the
##' two elements corresponding to the first and second model (not the
##' `mcstate_model`, but the underlying model, perhaps?).  This is the
##' only part that makes a distinction between the two models here;
##' for all components above they are equivalent.
##'
##' @title Combine two models
##'
##' @param a The first model
##'
##' @param b The second model
##'
##' @param properties An [mcstate_model_properties] object, used to
##'   control (or enforce) properties of the combined model.
##'
##' @param name_a Name of the first model (defaulting to 'a'); you can
##'   use this to make error messages nicer to read, but it has no
##'   other practical effect.
##'
##' @param name_b Name of the first model (defaulting to 'b'); you can
##'   use this to make error messages nicer to read, but it has no
##'   other practical effect.
##'
##' @return An [mcstate_model] object
##'
##' @export
mcstate_model_combine <- function(a, b, properties = NULL,
                                  name_a = "a", name_b = "b") {
  call <- environment()
  assert_is(a, "mcstate_model", name = name_a, call = call)
  assert_is(b, "mcstate_model", name = name_b, call = call)
  properties <- validate_model_properties(properties, call)

  parameters <- union(a$parameters, b$parameters)
  domain <- model_combine_domain(a, b, parameters)
  density <- model_combine_density(a, b, parameters)

  gradient <- model_combine_gradient(
    a, b, parameters, properties, call)
  direct_sample <- model_combine_direct_sample(
    a, b, parameters, properties, name_a, name_b, call)

  mcstate_model(
    list(model = list(a, b),
         parameters = parameters,
         domain = domain,
         density = density,
         gradient = gradient,
         direct_sample = direct_sample),
    properties)
}


##' @export
`+.mcstate_model` <- function(x, y) {
  if (!inherits(y, "mcstate_model")) {
    cli::cli_abort(
      paste("Addition via '+' is only defined for 'mcstate_model'",
            "with other 'mcstate_model' objects"))
  }
  mcstate_model_combine(x, y, properties = NULL, name_a = "lhs", name_b = "rhs")
}


model_combine_domain <- function(a, b, parameters) {
  domain <- matrix(NA_real_, length(parameters), 2)
  rownames(domain) <- parameters

  a_only <- setdiff(a$parameters, b$parameters)
  domain[a_only, ] <- a$domain[a_only, ]

  b_only <- setdiff(b$parameters, a$parameters)
  domain[b_only, ] <- b$domain[b_only, ]

  ## Shared parameters, take the most restrictive:
  shared <- intersect(a$parameters, b$parameters)
  domain[shared, 1] <- pmax(a$domain[shared, 1], b$domain[shared, 1])
  domain[shared, 2] <- pmin(a$domain[shared, 2], b$domain[shared, 2])

  domain
}


model_combine_density <- function(a, b, parameters) {
  i_a <- match(a$parameters, parameters)
  i_b <- match(b$parameters, parameters)
  function(x, ...) {
    a$density(x[i_a], ...) + b$density(x[i_b], ...)
  }
}


model_combine_gradient <- function(a, b, parameters, properties, call = NULL) {
  if (isFALSE(properties$has_gradient)) {
    return(NULL)
  }
  possible <- a$properties$has_gradient && b$properties$has_gradient
  required <- isTRUE(properties$has_gradient)
  if (!possible && !required) {
    return(NULL)
  }
  if (required && !possible) {
    cli::cli_abort(
      c("Can't create a gradient from these models",
        i = paste("Both models must have a gradient in order to be able",
                  "to create a gradient from the combination")),
      call = call)
  }

  n_pars <- length(parameters)
  i_a <- match(a$parameters, parameters)
  i_b <- match(b$parameters, parameters)
  function(x, ...) {
    ret <- numeric(n_pars)
    ret[i_a] <- ret[i_a] + a$gradient(x[i_a], ...)
    ret[i_b] <- ret[i_b] + b$gradient(x[i_b], ...)
    ret
  }
}


model_combine_direct_sample <- function(a, b, parameters, properties,
                                        name_a, name_b, call = NULL) {
  if (isFALSE(properties$has_direct_sample)) {
    return(NULL)
  }
  possible <- a$properties$has_direct_sample != b$properties$has_direct_sample
  required <- isTRUE(properties$has_direct_sample)
  if (!possible && !required) {
    return(NULL)
  }
  if (required && !possible) {
    if (a$properties$has_direct_sample) {
      hint <- paste("Both models have a 'direct_sample' method so we can't",
                    "combine them. Set 'has_direct_sample = FALSE' on one",
                    "of your models and try again")
    } else {
      hint <- "Neither of your models have 'direct_sample' methods"
    }
    cli::cli_abort(
      c("Can't create a direct_sample from these models",
        i = hint),
      call = call)
  }

  ## Next we need to check that the model with a sample has the full
  ## set of parameters
  if (a$properties$has_direct_sample) {
    name <- name_a
    model <- a
  } else {
    name <- name_b
    model <- b
  }
  if (!all(parameters %in% model$parameters)) {
    cli::cli_abort(
      paste("Can't create a direct_sample from these models as '{name}' does",
            "not contain all parameters in both models"),
      call = call)
  }
  i <- match(parameters, model$parameters)
  function(...) {
    model$direct_sample(...)[i]
  }
}
