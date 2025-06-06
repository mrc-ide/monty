% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random.R
\name{monty_random_hypergeometric}
\alias{monty_random_hypergeometric}
\alias{monty_random_n_hypergeometric}
\title{Sample from hypergeometric distribution}
\usage{
monty_random_hypergeometric(n1, n2, k, state)

monty_random_n_hypergeometric(n_samples, n1, n2, k, state)
}
\arguments{
\item{n1}{The number of white balls in the urn (called n in
R's \link{rhyper})}

\item{n2}{The number of black balls in the urn (called m in
R's \link{rhyper})}

\item{k}{The number of balls to draw}

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
Sample from a hypergeometric distribution.
}
