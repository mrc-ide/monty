##' Create a basic `mcstate` model.  Currently nothing here is
##' validated, and it's likely that users will never actually use this
##' directly.  Contains data and methods that define a basic model
##' object, so that we can implement samplers against.  Not all models
##' will support everything here, and we'll add additional
##' fields/traits over time to advertise what a model can do.  For
##' example, models will need to advertise that they are capable of
##' being differentiated, or that they are stochastic in order to be
##' used with different methods.
##'
##' @title Create basic model
##'
##' @param parameters Names of the parameters.  Every parameter is
##'   named, and for now every parameter is a scalar.  We might relax
##'   this later to support an `odin`-style structured parameter list,
##'   but that might just generate a suitable vector of parameter
##'   names perhaps?  In any case, once we start doing inference it's
##'   naturally in the R^n, and here n is defined as the length of
##'   this vector of names.
##'
##' @param sample A function to sample from the parameter space.  In
##'   the case where a model returns a posterior, this is assumed to
##'   be sampling from the prior.  We'll use this for generating
##'   initial conditions for MCMC where those are not given, and
##'   possibly other uses.
##'
##' @param density Compute the model density for a vector of parameter
##'   values; this is the posterior probability in the case of
##'   Bayesian inference, but it could be anything really.  Models can
##'   return `-Inf` if things are impossible, and we'll try and cope
##'   gracefully with that wherever possible.
##'
##' @param gradient Compute the gradient of `density` with respect to
##'   the parameter vector; takes a parameter vector and returns a
##'   vector the same length.  For efficiency, the model may want to
##'   be stateful so that gradients can be efficiently calculated
##'   after a density calculation, or density after gradient, where
##'   these are called with the same parameters.
##'
##' @param domain Information on the parameter domain.  This is a two
##'   column matrix with `length(parameters)` rows representing each
##'   parameter.  The parameter minimum and maximum bounds are given
##'   as the first and second column.  Infinite values (`-Inf` or
##'   `Inf`) should be used where the parameter has infinite domain up
##'   or down.  Currently used to translate from a bounded to
##'   unbounded space for HMC, but we might also use this for
##'   reflecting proposals in MCMC too.
##'
##' @return An object of class `mcstate_model`, which can be used with
##'   a sampler.
##'
##' @export
mcstate_model <- function(parameters, sample, density, gradient, domain) {
  ret <- list(parameters = parameters,
              sample = sample,
              density = density,
              gradient = gradient,
              domain = domain)
  class(ret) <- "mcstate_model"
  ret
}
