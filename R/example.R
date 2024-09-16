##' Load example models from monty.  These models exist so that we can
##' create (hopefully) interesting examples in the documentation
##' without them becoming overwhelming.  You should probably not use
##' these for anything other than exploring the package.
##'
##' # Supported models
##'
##' ## `bananna`
##'
##' The banana model is a two-dimensional banana-shaped function,
##' picked because it is quite annoying to sample from directly.  The
##' model has two parameters `alpha` and `beta` and is based on two
##' successive draws, one conditional on the other:
##'
##' \deqn{\beta \sim Normal(1, 0)}
##'
##' \deqn{\alpha \sim Normal(\beta^2, \sigma)}
##'
##' You can vary `sigma` for this model on creation, the default is 0.5
##'
##' @title Example models
##'
##' @param name Name of the example, as a string.  See Details for
##'   supported models.
##'
##' @param ... Optional parameters that are passed to create the
##'   model.  All models can be created with no additional parameters,
##'   but you can tweak their behaviour by passing named parameters
##'   here.  See Details.
##'
##' @return A [monty_model] object
##' @export
##'
##' @examples
##' monty_example("banana")
monty_example <- function(name, ...) {
  examples <- list(banana = monty_example_banana,
                   gaussian = monty_example_gaussian)
  examples[[match_value(name, names(examples))]](...)
}


monty_example_banana <- function(sigma = 0.5) {
  monty_model(
    list(
      parameters = c("alpha", "beta"),
      direct_sample = function(rng) {
        beta <- rng$random_normal(1)
        alpha <- rng$normal(1, beta^2, sigma)
        if (length(alpha) == 1) {
          c(alpha, beta)
        } else {
          rbind(alpha, beta)
        }
      },
      density = function(x) {
        if (is.matrix(x)) {
          alpha <- x[1, ]
          beta <- x[2, ]
        } else {
          alpha <- x[[1]]
          beta <- x[[2]]
        }
        dnorm(beta, log = TRUE) + dnorm((alpha - beta^2) / sigma, log = TRUE)
      },
      gradient = function(x) {
        if (is.matrix(x)) {
          alpha <- x[1, ]
          beta <- x[2, ]
        } else {
          alpha <- x[[1]]
          beta <- x[[2]]
        }
        da <- (beta^2 - alpha) / sigma^2
        db <- -beta + 2 * beta * (alpha - beta^2) / sigma^2
        if (is.matrix(x)) {
          rbind(da, db, deparse.level = 0)
        } else {
          c(da, db)
        }
      },
      domain = rbind(c(-Inf, Inf), c(-Inf, Inf))),
    properties = monty_model_properties(allow_multiple_parameters = TRUE))
}


monty_example_gaussian <- function(vcv) {
  n <- nrow(vcv)
  monty_model(list(
    parameters = letters[seq_len(n)],
    direct_sample = make_rmvnorm(vcv, centred = TRUE),
    density = make_ldmvnorm(vcv),
    gradient = make_deriv_ldmvnorm(vcv),
    domain = cbind(rep(-Inf, n), rep(Inf, n))))
}
