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
##' * `is_stochastic`: a model is stochastic if *either* component is
##'   stochastic.
##'
##' The properties of the model will be combined as above, reflecting
##' the properties of the joint model.
##'
##' The `model` field will be an ordered, unnamed, list containing the
##' two elements corresponding to the first and second model (not the
##' `monty_model`, but the underlying model, perhaps?).  This is the
##' only part that makes a distinction between the two models here;
##' for all components above they are equivalent.
##'
##' @title Combine two models
##'
##' @param a The first model
##'
##' @param b The second model
##'
##' @param properties A [monty_model_properties] object, used to
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
##' @return A [monty_model] object
##'
##' @export
##' @examples
##' # A simple example; a model that contains something of interest,
##' # and a simple prior from monty_dsl
##' likelihood <- monty_example("banana")
##' prior <- monty_dsl({
##'   alpha ~ Normal(0, 1)
##'   beta ~ Normal(0, 10)
##' })
##' posterior <- likelihood + prior
##' posterior
##'
##' # The same thing, more explicitly:
##' monty_model_combine(likelihood, prior)
##'
##' # Control properties of the combined model:
##' monty_model_combine(likelihood, prior,
##'                     monty_model_properties(has_gradient = FALSE))
monty_model_combine <- function(a, b, properties = NULL,
                                name_a = "a", name_b = "b") {
  call <- environment()
  require_monty_model(a)
  require_monty_model(b)
  properties <- validate_model_properties(properties, call)

  parameters <- union(a$parameters, b$parameters)

  parts <- set_names(list(a, b), c(name_a, name_b))
  is_prior <- vlapply(parts, function(el) {
    el$properties$has_direct_sample &&
      !el$properties$is_stochastic &&
      all(el$parameters %in% parameters)
  })

  one_is_prior <- sum(is_prior) == 1
  if (one_is_prior && is_prior[[2]]) {
    parts <- parts[2:1]
    is_prior <- is_prior[2:1]
  }

  properties$allow_multiple_parameters <-
    model_combine_allow_multiple_parameters(parts, properties)

  domain <- model_combine_domain(parts, parameters)
  density <- model_combine_density(parts, parameters,
                                   properties$allow_multiple_parameters)

  gradient <- model_combine_gradient(
    parts, parameters, properties, call)
  direct_sample <- model_combine_direct_sample(
    parts, parameters, properties, call)
  stochastic <- model_combine_stochastic(
    parts, properties)
  observer <- model_combine_observer(
    parts, parameters, properties, call)
  restore <- model_combine_restore(
    parts)

  data <- list(
    parts = unname(parts),
    is_prior = unname(is_prior))
  class(data) <- "_combined_model"

  monty_model(
    list(data = data,
         parameters = parameters,
         domain = domain,
         density = density,
         gradient = gradient,
         get_rng_state = stochastic$get_rng_state,
         set_rng_state = stochastic$set_rng_state,
         restore = restore,
         observer = observer,
         direct_sample = direct_sample),
    properties)
}


##' @export
`+.monty_model` <- function(x, y) {
  if (!inherits(y, "monty_model")) {
    cli::cli_abort(
      paste("Addition via '+' is only defined for 'monty_model'",
            "with other 'monty_model' objects"))
  }
  monty_model_combine(x, y, properties = NULL, name_a = "lhs", name_b = "rhs")
}


##' Split a model that has been combined by [monty_model_combine()] into
##' its constituent parts.
##'
##' We assume that a split model can be broken into a "prior" and a
##' "likelihood" if exactly one model:
##'
##' * can be directly sampled from
##' * is not stochastic
##' * consumes all parameters
##'
##' Typically, it will be the first criterion that will separate a
##' model into prior and likelihood (if you could sample from your
##' likelihood, then you would not use a sampler, which is where we
##' are typically going to perform this action).
##'
##' If `prior_first` is `FALSE` we just return the parts.
##'
##' @title Split a combined model
##'
##' @param model A combined model
##'
##' @param prior_first Logical, indicating that we should require that
##'   the model component that could be the prior is listed first.  If
##'   `TRUE` and one component model is not plausibly the prior, we
##'   will error.  See Details for the heuristic used.
##'
##' @return An unnamed list of length 2, being the component models.
##'   If one model might be the prior it will be listed first.
##'
##' @export
monty_model_split <- function(model, prior_first = FALSE) {
  require_monty_model(model, arg = "model", call = call)

  if (!inherits(model$model$data, "_combined_model")) {
    cli::cli_abort(
      "Cannot split this model as it is not a combined model")
  }

  if (prior_first) {
    is_prior <- model$model$data$is_prior
    if (!any(is_prior)) {
      cli::cli_abort(
        "Neither model component looks like a prior")
    }
    if (all(is_prior)) {
      cli::cli_abort(
        "Either model component could be the prior")
    }
  }

  model$model$data$parts
}


model_combine_domain <- function(parts, parameters) {
  a <- parts[[1]]
  b <- parts[[2]]
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


model_combine_density <- function(parts, parameters, allow_multiple_parameters) {
  a <- parts[[1]]
  b <- parts[[2]]
  i_a <- match(a$parameters, parameters)
  i_b <- match(b$parameters, parameters)
  if (allow_multiple_parameters) {
    function(x, ...) {
      if (is.matrix(x)) {
        a$density(x[i_a, , drop = FALSE], ...) +
          b$density(x[i_b, , drop = FALSE], ...)
      } else {
        a$density(x[i_a], ...) + b$density(x[i_b], ...)
      }
    }
  } else {
    function(x, ...) {
      a$density(x[i_a], ...) + b$density(x[i_b], ...)
    }
  }
}


model_combine_stochastic <- function(parts, properties, call = NULL) {
  a <- parts[[1]]
  b <- parts[[2]]
  a_stochastic <- a$properties$is_stochastic
  b_stochastic <- b$properties$is_stochastic

  if (!a_stochastic && !b_stochastic) {
    if (isTRUE(properties$is_stochastic)) {
      cli::cli_abort(
        c("Can't create stochastic support functions for these models",
          i = paste("Neither of your models as stochastic, but you have",
                    "requested that we create a stochastic model by",
                    "providing the property 'is_stochastic = TRUE'")),
        call = call)
    } else {
      return(NULL)
    }
  }

  if (a_stochastic && b_stochastic) {
    ## This branch is hard because we need to set the state into
    ## both models, but that is going to require advancing the state
    ## or using it directly (which we don't really want people to
    ## do).  To combine stochastic models, the user will have to do
    ## more work themselves, or we can work out what that looks like
    ## in practice and provide a hook to expand the state as
    ## required.
    cli::cli_abort(
      c("Can't combine two stochastic models",
        i = paste("We can't create stochastic support functions for a",
                  "model that combines two stochastic models, as we",
                  "don't correctly spread the random number state across",
                  "the models.  We would need to know a bit more about",
                  "how the random numbers are used in the models.",
                  "Please let us know this is something you",
                  "would like to do and we'll see what can be done")),
      call = call)
  }

  if (isFALSE(properties$is_stochastic)) {
    cli::cli_abort(
      c("Refusing to create non-stochastic model from stochastic components",
        i = paste("One of your models is stochastic, but you have passed",
                  "the property 'is_stochastic = FALSE'; I can't build a",
                  "model out of this. Because your component models are",
                  "stochastic, the combination must be stochastic and you",
                  "cannot just assert that away")),
      call = call)
  }

  m <- if (a_stochastic) a else b
  list(get_rng_state = m$rng_state$get,
       set_rng_state = m$rng_state$set)
}


model_combine_gradient <- function(parts, parameters, properties, call = NULL) {
  a <- parts[[1]]
  b <- parts[[2]]
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

  gradient_vector <- function(x, ...) {
    ret <- numeric(n_pars)
    ret[i_a] <- ret[i_a] + a$gradient(x[i_a], ...)
    ret[i_b] <- ret[i_b] + b$gradient(x[i_b], ...)
    ret
  }

  if (properties$allow_multiple_parameters) {
    function(x, ...) {
      if (is.matrix(x)) {
        ret <- matrix(0, n_pars, ncol(x))
        ret[i_a, ] <-
          ret[i_a, , drop = FALSE] + a$gradient(x[i_a, , drop = FALSE], ...)
        ret[i_b, ] <-
          ret[i_b, , drop = FALSE] + b$gradient(x[i_b, , drop = FALSE], ...)
        ret
      } else {
        gradient_vector(x, ...)
      }
    }
  } else {
    gradient_vector
  }
}


model_combine_direct_sample <- function(parts, parameters, properties,
                                        call = NULL) {
  a <- parts[[1]]
  b <- parts[[2]]
  name_a <- names(parts)[[1]]
  name_b <- names(parts)[[2]]
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


model_combine_observer <- function(parts, parameters, properties, call = NULL) {
  a <- parts[[1]]
  b <- parts[[2]]
  name_a <- names(parts)[[1]]
  name_b <- names(parts)[[2]]
  if (isFALSE(properties$has_observer)) {
    return(NULL)
  }
  possible <- a$properties$has_observer != b$properties$has_observer
  required <- isTRUE(properties$has_observer)
  if (!possible && !required) {
    return(NULL)
  }
  if (required && !possible) {
    if (a$properties$has_observer) {
      hint <- paste("Both models have an 'observer' object so we can't",
                    "combine them. Set 'has_observer = FALSE' on one",
                    "of your models and try again")
    } else {
      hint <- "Neither of your models have 'observer' objects"
    }
    cli::cli_abort(
      c("Can't create an observer from these models",
        i = hint),
      call = call)
  }

  model <- if (a$properties$has_observer) a else b
  model$observer
}


model_combine_allow_multiple_parameters <- function(parts, properties,
                                                    call = parent.frame()) {
  a <- parts[[1]]
  b <- parts[[2]]
  if (isFALSE(properties$allow_multiple_parameters)) {
    return(FALSE)
  }
  possible <- a$properties$allow_multiple_parameters &&
    b$properties$allow_multiple_parameters
  if (possible) {
    return(TRUE)
  }
  required <- isTRUE(properties$allow_multiple_parameters)
  if (!required) {
    return(FALSE)
  }
  cli::cli_abort(
    paste("Can't specify 'allow_multiple_parameters = TRUE' as this is",
          "not supported by both of your models"),
    call = call)
}


model_combine_restore <- function(parts) {
  a <- parts[[1]]
  b <- parts[[2]]
  function() {
    a$restore()
    b$restore()
  }
}
