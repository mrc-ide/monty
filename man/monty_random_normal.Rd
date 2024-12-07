% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random.R
\name{monty_random_normal}
\alias{monty_random_normal}
\alias{monty_random_n_normal}
\title{Sample from normal distribution}
\usage{
monty_random_normal(mean, sd, state)

monty_random_n_normal(n_samples, mean, sd, state)
}
\arguments{
\item{mean}{The mean of the normal distribution}

\item{sd}{The standard deviation of the normal distribution}

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
Sample from a normal distribution
}
