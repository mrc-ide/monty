##' Create a basic `mcstate` model.  This takes a user-supplied object
##' that minimally can compute a probability density (via a `density`
##' function) and information about parameters; with this we can
##' sample from the model using `MCMC` using [mcstate_sample].  We
##' don't imagine that many users will call this function directly,
##' but that this will be glue used by packages.
##'
##' The `model` argument can be a list or environment (something
##' indexable by `$`) and have elements:
##'
##' * `density`: A function that will compute some probability
##'   density.  It must take an argument representing a parameter
##'   vector (a numeric vector) and return a single value.  This is
##'   the posterior probability density in Bayesian inference, but it
##'   could be anything really.  Models can return `-Inf` if things
##'   are impossible, and we'll try and cope gracefully with that
##'   wherever possible.
##'
##' * `parameters`: A character vector of parameter names.  This can
##'   be overridden by the `parameters` argument to this function.
##'   This vector is the source of truth for the length of the
##'   parameter vector.
##'
##' * `domain`: Information on the parameter domain.  This can be
##'   overridden by the `domain` argument to this function.  This is a
##'   two column matrix with `length(parameters)` rows representing
##'   each parameter.  The parameter minimum and maximum bounds are
##'   given as the first and second column.  Infinite values (`-Inf`
##'   or `Inf`) should be used where the parameter has infinite domain
##'   up or down.  Currently used to translate from a bounded to
##'   unbounded space for HMC, but we might also use this for
##'   reflecting proposals in MCMC too.  If not present (and if not
##'   overridden by `domain`) we assume that the model is valid
##'   everywhere (i.e., that all parameters are valid from `-Inf` to
##'   `Inf`.
##'
##' * `direct_sample`: A function to sample directly from the
##'   parameter space, given an [mcstate_rng] object to sample from.
##'   In the case where a model returns a posterior (e.g., in Bayesian
##'   inference), this is assumed to be sampling from the prior.
##'   We'll use this for generating initial conditions for MCMC where
##'   those are not given, and possibly other uses.  If not given then
##'   when using [mcstate_sample()] the user will have to provide a
##'   vector of initial states.
##'
##' * `gradient`: A function to compute the gradient of `density` with
##'   respect to the parameter vector; takes a parameter vector and
##'   returns a vector the same length.  For efficiency, the model may
##'   want to be stateful so that gradients can be efficiently
##'   calculated after a density calculation, or density after
##'   gradient, where these are called with the same parameters.  This
##'   function is optional (and may not be well defined or possible to
##'   define).
##'
##' @title Create basic model
##'
##' @param model A list or environment with elements as described in
##'   Details.
##'
##' @param parameters A character vector of parameter names, for cases
##'   where `model` does not provide this or where you want to
##'   override parameter names. This is required if `model$parameters`
##'   is `NULL`.  If `parameters` is given and `model$parameters` is
##'   not NULL we enforce that they are the same length.
##'
##' @param domain Information on the parameter domain.  This is a two
##'   column matrix with `length(parameters)` rows representing each
##'   parameter.  The parameter minimum and maximum bounds are given
##'   as the first and second column.  Infinite values (`-Inf` or
##'   `Inf`) should be used where the parameter has infinite domain up
##'   or down.  Currently used to translate from a bounded to
##'   unbounded space for HMC, but we might also use this for
##'   reflecting proposals in MCMC too.  This must be given if not
##'   provided by the model (i.e., if `model$domain` is `NULL`) and if
##'   given overrides any value present in the model.
##'
##' @return An object of class `mcstate_model`.  This will have elements:
##'
##' * `model`: The model as provided
##' * `parameters`: The parameter name vector
##' * `domain`: The parameter domain matrix
##' * `properties`: A list of properties of the model; we'll grow this
##'   over time but this currently contains:
##'     * `has_gradient`: the model can compute its gradient
##'     * `has_direct_sample`: the model can sample from parameters space
##'
##' @export
mcstate_model <- function(model, parameters = NULL, domain = NULL) {
  if (is.null(parameters)) {
    if (!is.character(model$parameters)) {
      cli::cli_abort(
        paste("Expected 'model$parameters' to be a character vector, as",
              "'parameters' was not given"))
    }
    parameters <- model$parameters
  } else if (!is.character(parameters)) {
    cli::cli_abort("Expected 'parameters' to be a character vector")
  }
  n_pars <- length(parameters)

  if (!is.function(model$density)) {
    cli::cli_abort("Expected 'model$density' to be a function")
  }

  ## We might allow `has_gradient = FALSE` here to allow ignoring this
  if (!is.null(model$gradient) && !is.function(model$gradient)) {
    cli::cli_abort("Expected 'model$gradient' to be a function if non-NULL")
  }

  if (!is.null(model$direct_sample) && !is.function(model$direct_sample)) {
    cli::cli_abort(
      "Expected 'model$direct_sample' to be a function if non-NULL")
  }

  if (is.null(domain) && is.null(model$domain)) {
    domain <- cbind(rep(-Inf, n_pars), rep(Inf, n_pars))
  } else {
    if (is.null(domain)) {
      domain <- model$domain
      domain_src <- "model$domain"
    } else {
      domain_src <- "domain"
    }
    if (!is.matrix(domain)) {
      cli::cli_abort("Expected '{domain_src}' to be a matrix if non-NULL")
    }
    if (nrow(domain) != n_pars) {
      cli::cli_abort(paste(
        "Expected '{domain_src}' to have {n_pars} row{?s},",
        "but it had {nrow(domain)}"))
    }
    if (ncol(domain) != 2) {
      cli::cli_abort(paste(
        "Expected '{domain_src}' to have 2 columns,",
        "but it had {ncol(domain)}"))
    }
  }

  properties <- list(has_gradient = !is.null(model$gradient),
                     has_direct_sample = !is.null(model$direct_sample))
  ret <- list(model = model,
              parameters = parameters,
              domain = domain,
              properties = properties)
  class(ret) <- "mcstate_model"
  ret
}


require_direct_sample <- function(model, message, ...) {
  if (!model$properties$has_direct_sample) {
    cli::cli_abort(
      c(message,
        i = paste("This model does not provide 'direct_sample()', so we",
                  "cannot directly sample from its parameter space")),
      ...)
  }
}
