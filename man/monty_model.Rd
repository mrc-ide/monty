% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{monty_model}
\alias{monty_model}
\title{Create basic model}
\usage{
monty_model(model, properties = NULL)
}
\arguments{
\item{model}{A list or environment with elements as described in
Details.}

\item{properties}{Optionally, a \link{monty_model_properties} object,
used to enforce or clarify properties of the model.}
}
\value{
An object of class \code{monty_model}.  This will have elements:
\itemize{
\item \code{model}: The model as provided
\item \code{parameters}: The parameter name vector
\item \code{parameter_groups}: The parameter groups
\item \code{domain}: The parameter domain matrix, named with your parameters
\item \code{direct_sample}: The \code{direct_sample} function, if provided by the model
\item \code{gradient}: The \code{gradient} function, if provided by the model
\item \code{properties}: A list of properties of the model;
see \code{\link[=monty_model_properties]{monty_model_properties()}}.  Currently this contains:
\itemize{
\item \code{has_gradient}: the model can compute its gradient
\item \code{has_direct_sample}: the model can sample from parameters space
\item \code{is_stochastic}: the model will behave stochastically
\item \code{has_parameter_groups}: The model has separable parameter groups
}
}
}
\description{
Create a basic \code{monty} model.  This takes a user-supplied object
that minimally can compute a probability density (via a \code{density}
function) and information about parameters; with this we can
sample from the model using \code{MCMC} using \link{monty_sample}.  We
don't imagine that many users will call this function directly,
but that this will be glue used by packages.
}
\details{
The \code{model} argument can be a list or environment (something
indexable by \code{$}) and have elements:
\itemize{
\item \code{density}: A function that will compute some probability
density.  It must take an argument representing a parameter
vector (a numeric vector) and return a single value.  This is
the posterior probability density in Bayesian inference, but it
could be anything really.  Models can return \code{-Inf} if things
are impossible, and we'll try and cope gracefully with that
wherever possible.  If the property \code{allow_multiple_parameters}
is \code{TRUE}, then this function must be able to handle the
argument parameter being a matrix,  and return a vector
of densities.
\item \code{parameters}: A character vector of parameter names.  This
vector is the source of truth for the length of the parameter
vector.
\item \code{domain}: Information on the parameter domain.  This is a two
column matrix with \code{length(parameters)} rows representing each
parameter.  The parameter minimum and maximum bounds are given
as the first and second column.  Infinite values (\code{-Inf} or
\code{Inf}) should be used where the parameter has infinite domain up
or down.  Currently used to translate from a bounded to
unbounded space for HMC, but we might also use this for
reflecting proposals in MCMC too, as well as a fast way of
avoiding calculating densities where proposals fall out of
bounds.  If not present we assume that the model is valid
everywhere (i.e., that all parameters are valid from \code{-Inf} to
\code{Inf}.  If unnamed, you must provide a domain for all
parameters.  If named, then you can provide a subset, with
parameters that are not included assumed to have a domain of
\verb{(-Inf, Inf)}.
\item \code{direct_sample}: A function to sample directly from the
parameter space, given a \code{monty_rng} object to sample from (see
\link{monty_rng_create}).  In the case where a model returns a
posterior (e.g., in Bayesian inference), this is assumed to be
sampling from the prior.  We'll use this for generating initial
conditions for MCMC where those are not given, and possibly
other uses.  If not given then when using \code{\link[=monty_sample]{monty_sample()}} the
user will have to provide a vector of initial states.
\item \code{gradient}: A function to compute the gradient of \code{density} with
respect to the parameter vector; takes a parameter vector and
returns a vector the same length.  For efficiency, the model may
want to be stateful so that gradients can be efficiently
calculated after a density calculation, or density after
gradient, where these are called with the same parameters.  This
function is optional (and may not be well defined or possible to
define).
\item \code{set_rng_state}: A function to set the state (this is in
contrast to the \code{rng} that is passed through to \code{direct_sample}
as that is the \emph{sampler's} rng stream, but we assume models will
look after their own stream, and that they may need many
streams).  Models that provide this method are assumed to be
stochastic; however, you can use the \code{is_stochastic} property
(via \code{\link[=monty_model_properties]{monty_model_properties()}}) to override this (e.g., to run
a stochastic model with its deterministic expectation).  This
function takes a raw vector of random number state from a
\code{monty_rng} and uses it to set the random number state for your
model; this is derived from the random number stream for a
particular chain, jumped ahead.
\item \code{get_rng_state}: A function to get the RNG state; must be
provided if \code{set_rng_state} is present.  Must return the random
number state, which is a raw vector (potentially quite long).
\item \code{parameter_groups}: Optionally, an integer vector indicating
parameter group membership.  The format here may change
(especially if we explore more complex nestings) but at present
parameters with group 0 affect everything (so are accepted or
rejected as a whole), while parameters in groups 1 to \code{n} are
independent (for example, changing the parameters in group 2 does
not affect the density of parameters proposed in group 3).
}
}
\seealso{
\link{monty_model_function}, which provides a simple interface
for creating models from functions.
}
