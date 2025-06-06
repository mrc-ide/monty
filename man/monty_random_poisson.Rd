% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random.R
\name{monty_random_poisson}
\alias{monty_random_poisson}
\alias{monty_random_n_poisson}
\title{Sample from Poisson distribution}
\usage{
monty_random_poisson(lambda, state)

monty_random_n_poisson(n_samples, lambda, state)
}
\arguments{
\item{lambda}{The mean (zero or more, length 1 or n). Only valid for
lambda <= 10^7}

\item{state}{The random number state, from \link{monty_rng_create}}

\item{n_samples}{The number of samples to take, \strong{per stream}.
When using the multiple-sample interface, all other parameters
are held constant (per stream).}
}
\value{
A vector of random numbers, the same length as the number
of streams in \code{state}.
}
\description{
Sample from the Poisson distribution
}
