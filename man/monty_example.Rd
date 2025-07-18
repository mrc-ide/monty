% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example.R
\name{monty_example}
\alias{monty_example}
\title{Example models}
\usage{
monty_example(name, ...)
}
\arguments{
\item{name}{Name of the example, as a string.  See Details for
supported models.}

\item{...}{Optional parameters that are passed to create the
model.  All models can be created with no additional parameters,
but you can tweak their behaviour by passing named parameters
here.  See Details.}
}
\value{
A \link{monty_model} object
}
\description{
Load example models from monty.  These models exist so that we can
create (hopefully) interesting examples in the documentation
without them becoming overwhelming.  You should probably not use
these for anything other than exploring the package.
}
\section{Supported models}{
\subsection{\code{banana}}{

The banana model is a two-dimensional banana-shaped function,
picked because it is quite annoying to sample from directly.  The
model has two parameters \code{alpha} and \code{beta} and is based on two
successive draws, one conditional on the other.

You can vary \code{sigma} for this model on creation, the default is 0.5
}

\subsection{Gaussian}{

A multivariate Gaussian centred at the origin.  Takes a
variance-covariance-matrix as its argument.  Parameters are
letters a, b, ... up to the number of dimensions.
}
}

\examples{
monty_example("banana")
monty_example("gaussian", diag(2))
}
