##' Create a monty random number generator.  This allows you to sample
##' random numbers from the same random number algorithms as monty
##' provides via C++ to [dust2](https://mrc-ide.github.io/dust2), and
##' which it uses within its samplers and filters.  This function
##' creates the internal state, and will be passed to actual
##' generation functions `monty_random_*`, such as
##' [monty_random_real()].
##'
##' Monty's random number generation is very different to that in base
##' R.  We have the concept of "streams" of random numbers, with a
##' generator having 1 or many streams.  Each stream is statistically
##' independent, and can be sampled from simultaneously.  When you use
##' any of the random number functions from R, you will draw one
##' number *per stream*.
##'
##' Because the random number state can have multiple streams, and we
##' return a vector, we have a separate set of functions where
##' multiple numbers are requested *per stream*; these are all
##' prefixed `monty_random_n_` (e.g., [monty_random_n_real()]).  These
##' will return a matrix where you have used multiple streams, with
##' each column representing a stream.  If you have a single stream
##' and you set `preserve_stream_dimension` to `FALSE` then we will
##' drop this dimension and return a matrix.
##'
##' @title Create a monty random number genreator
##'
##' @param n_streams The number of streams to create (see Details)
##'
##' @param seed The initial seed of the random number generator.  This
##'   can be `NULL`, in which case we seed the generator from R's
##'   random number state (meaning that we respond to `set.seed` as
##'   one would expect).  Alternatively, you can provide an integer
##'   here, but this should be used sparingly and primarily for
##'   testing.
##'
##' @param n_threads The number of threads to use, if OpenMP is
##'   enabled.
##'
##' @param deterministic Logical, indicating if we should use
##'   "deterministic" mode where distributions return their
##'   expectations and the state is never changed.
##'
##' @param preserve_stream_dimension Logical, indicating if the stream
##'   dimension should be preserved in the case where `n_streams` is 1
##'   and the multiple-sample functions are used.  Set this to `TRUE`
##'   to ensure that the rank of the result does not change with the
##'   number of streams (see Details).
##'
##' @return An object of class `monty_rng_state`, which can be
##'   passed as the `state` argument to random-number producing
##'   functions, such as [monty_random_real]
##'
##' @export
##' @examples
##' state <- monty_rng_create()
##' state
##'
##' monty_random_real(state)
monty_rng_create <- function(n_streams = 1L, seed = NULL,
                                n_threads = 1L, deterministic = FALSE,
                                preserve_stream_dimension = FALSE) {
  assert_scalar_logical(deterministic)
  assert_scalar_logical(preserve_stream_dimension)
  preserve_stream_dimension <- preserve_stream_dimension || n_streams > 1
  ptr <- monty_rng_alloc(seed, n_streams, deterministic)
  attr(ptr, "n_streams") <- n_streams
  attr(ptr, "n_threads") <- n_threads
  attr(ptr, "n_deterministic") <- deterministic
  attr(ptr, "preserve_stream_dimension") <- preserve_stream_dimension
  class(ptr) <- "monty_rng_state"
  ptr
}


##' Get and set internal random number state
##'
##' @title Get and set random number state
##'
##' @param state The random number state, from [monty_rng_create]
##'
##' @return A vector of raws
##' @export
##' @examples
##' s1 <- monty_rng_create()
##' r1 <- monty_rng_state(s1)
##'
##' s2 <- monty_rng_create(seed = r1)
##' identical(r1, monty_rng_state(s2))
##' monty_random_real(s1)
##' monty_random_real(s2)
##'
##' monty_rng_set_state(r1, s1)
##' monty_random_real(s1)
##' monty_random_real(s1)
##' monty_random_real(s2)
monty_rng_state <- function(state) {
  cpp_monty_rng_state(state)
}


##' @param value A vector of raw values, typically the result of
##'   exporting a random state with `monty_rng_state()`
##'
##' @export
##' @rdname monty_rng_state
monty_rng_set_state <- function(value, state) {
  cpp_monty_rng_set_state(state, value)
}


##' Jump random number state.  There are two "lengths" of jumps; a
##' normal jump and a long jump.  The normal jump is the distance
##' between streams within a random number state, so if you have a
##' multi-stream rng this shifts states left.  The long jump is used
##' to create distributed states.  We will properly explain all this
##' once the interface stabilises.
##'
##' @title Jump random number state
##'
##' @param state Either a `monty_rng_state` object (created via
##'   [monty_rng_create]) or a raw vector suitable for creating
##'   one.
##'
##' @param n The number of jumps to take (integer, 1 or more)
##'
##' @return The `monty_rng_state` object (invisibly, modified in
##'   place) or a raw vector, matching the input argument `state`
##'   (visibly).
##'
##' @export
monty_rng_jump <- function(state, n = 1) {
  if (is.raw(state)) {
    rng <- monty_rng_create(seed = state, n_streams = length(state) %/% 32)
    monty_rng_state(monty_rng_jump(rng, n))
  } else {
    assert_is(state, "monty_rng_state")
    assert_scalar_size(n, allow_zero = FALSE)
    cpp_monty_rng_jump(state, n)
    invisible(state)
  }
}


##' @export
##' @rdname monty_rng_jump
monty_rng_long_jump <- function(state, n = 1) {
  if (is.raw(state)) {
    rng <- monty_rng_create(seed = state, n_streams = length(state) %/% 32)
    monty_rng_state(monty_rng_long_jump(rng, n))
  } else {
    assert_is(state, "monty_rng_state")
    assert_scalar_size(n, allow_zero = FALSE)
    cpp_monty_rng_long_jump(state, n)
    invisible(state)
  }
}



##' @export
print.monty_rng_state <- function(x, ...) {
  cli::cli_h1("<monty_rng_state>")
  cli::cli_li("{length(x)} random number stream{?s}")
  cli::cli_li("{attr(x, 'n_threads')} execution thread{?s}")
  invisible(x)
}


##' @export
length.monty_rng_state <- function(x) {
  attr(x, "n_streams")
}


##' Generate a random number uniformly sampled on the range 0 to 1;
##' this is the most basic of all random number functions in monty and
##' all other algorithms are composed from this.  Quite often, you
##' will want a number on $\[0, 1\]$ (e.g., for a Bernoulli trial), and
##' this function is the most efficient way of generating one.
##'
##' @title Sample from Uniform(0, 1)
##'
##' @param state The random number state, from [monty_rng_create]
##'
##' @return A vector of random numbers, the same length as the number
##'   of streams in `state`.
##'
##' @export
##' @examples
##' state <- monty_rng_create()
##' monty_random_real(state)
##' monty_random_n_real(5, state)
monty_random_real <- function(state) {
  cpp_monty_random_real(state)
}


##' @param n_samples The number of samples to take, **per stream**.
##'   When using the multiple-sample interface, all other parameters
##'   are held constant (per stream).
##'
##' @export
##' @rdname monty_random_real
monty_random_n_real <- function(n_samples, state) {
  cpp_monty_random_n_real(n_samples, state)
}


##' Sample from an exponential distribution.  There are two
##' parameterisations here, one in terms of the rate of the
##' exponential, and one in terms of the mean (or scale).
##'
##' @title Sample from exponential distribution
##'
##' @param rate The rate of the exponential
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
##' @rdname monty_random_exponential
##' @examples
##' state <- monty_rng_create()
##' monty_random_exponential_rate(0.2, state)
##' summary(monty_random_n_exponential_rate(2000, 0.2, state))
monty_random_exponential_rate <- function(rate, state) {
  cpp_monty_random_exponential_rate(rate, state)
}


##' @export
##' @rdname monty_random_exponential
monty_random_n_exponential_rate <- function(n_samples, rate, state) {
  cpp_monty_random_n_exponential_rate(n_samples, rate, state)
}


##' @param mean The mean of the exponential distribution (i.e., `1 / rate`)
##' @export
##' @rdname monty_random_exponential
monty_random_exponential_mean <- function(mean, state) {
  cpp_monty_random_exponential_mean(mean, state)
}


##' @export
##' @rdname monty_random_exponential
monty_random_n_exponential_mean <- function(n_samples, mean, state) {
  cpp_monty_random_n_exponential_mean(n_samples, mean, state)
}


##' Sample from the Poisson distribution
##'
##' @title Sample from Poisson distribution
##'
##' @param lambda The mean (zero or more, length 1 or n). Only valid for
##' lambda <= 10^7
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##' @export
monty_random_poisson <- function(lambda, state) {
  cpp_monty_random_poisson(lambda, state)
}


##' @export
##' @rdname monty_random_poisson
monty_random_n_poisson <- function(n_samples, lambda, state) {
  cpp_monty_random_n_poisson(n_samples, lambda, state)
}


##' Sample from the beta distribution
##'
##' @title Sample from beta distribution
##'
##' @param a,b The shape parameters
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##' @export
monty_random_beta <- function(a, b, state) {
  cpp_monty_random_beta(a, b, state)
}


##' @export
##' @rdname monty_random_beta
monty_random_n_beta <- function(n_samples, a, b, state) {
  cpp_monty_random_n_beta(n_samples, a, b, state)
}


##' Sample from the binomial distribution
##'
##' @title Sample from binomial distribution
##'
##' @param size The number of trials
##'
##' @param prob The probability of success on each trial
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
##' @examples
##' state <- monty_rng_create()
##' monty_random_binomial(10, 0.3, state)
##' table(monty_random_n_binomial(2000, 10, 0.3, state))
monty_random_binomial <- function(size, prob, state) {
  cpp_monty_random_binomial(size, prob, state)
}


##' @export
##' @rdname monty_random_binomial
monty_random_n_binomial <- function(n_samples, size, prob, state) {
  cpp_monty_random_n_binomial(n_samples, size, prob, state)
}


##' Sample from the Cauchy distribution
##'
##' @title Sample from Cauchy distribution
##'
##' @param location Location of the distribution (the same as the
##'   median and mode)
##'
##' @param scale A scale parameter which specifies the half-width at
##'   half-maximum (HWHM)
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##' @export
monty_random_cauchy <- function(location, scale, state) {
  cpp_monty_random_cauchy(location, scale, state)
}


##' @export
##' @rdname monty_random_cauchy
monty_random_n_cauchy <- function(n_samples, location, scale, state) {
  cpp_monty_random_n_cauchy(n_samples, location, scale, state)
}


##' Sample from a gamma distribution.  There are two parameterisations
##' here, one in terms of rate, and one in terms of scale.
##'
##' @description Sample from gamma distribution
##'
##' @param shape Shape
##'
##' @param scale Scale
##''
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
##' @rdname monty_random_gamma
monty_random_gamma_scale <- function(shape, scale, state) {
  cpp_monty_random_gamma_scale(shape, scale, state)
}


##' @export
##' @rdname monty_random_gamma
monty_random_n_gamma_scale <- function(n_samples, shape, scale, state) {
  cpp_monty_random_n_gamma_scale(n_samples, shape, scale, state)
}


##' @param rate Rate
##' @export
##' @rdname monty_random_gamma
monty_random_gamma_rate <- function(shape, rate, state) {
  cpp_monty_random_gamma_rate(shape, rate, state)
}


##' @export
##' @rdname monty_random_gamma
monty_random_n_gamma_rate <- function(n_samples, shape, rate, state) {
  cpp_monty_random_n_gamma_rate(n_samples, shape, rate, state)
}


##' Sample from a negative binomial distribution
##'
##' @title Sample from negative binomial distribution
##'
##' @param size The target number of successful trials
##'   (zero or more)
##'
##' @param prob The probability of success on each trial (between 0
##'   and 1)
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
##' @rdname monty_random_negative_binomial
monty_random_negative_binomial_prob <- function(size, prob, state) {
  cpp_monty_random_negative_binomial_prob(size, prob, state)
}


##' @export
##' @rdname monty_random_negative_binomial
monty_random_n_negative_binomial_prob <- function(n_samples, size, prob,
                                                  state) {
  cpp_monty_random_n_negative_binomial_prob(n_samples, size, prob, state)
}



##' @param mu The mean (zero or more)
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
##' @rdname monty_random_negative_binomial
monty_random_negative_binomial_mu <- function(size, mu, state) {
  cpp_monty_random_negative_binomial_mu(size, mu, state)
}


##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
##' @rdname monty_random_negative_binomial
monty_random_n_negative_binomial_mu <- function(n_samples, size, mu, state) {
  cpp_monty_random_n_negative_binomial_mu(n_samples, size, mu, state)
}


##' Sample from a normal distribution
##'
##' @title Sample from normal distribution
##'
##' @param mean The mean of the normal distribution
##'
##' @param sd The standard deviation of the normal distribution
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
monty_random_normal <- function(mean, sd, state) {
  cpp_monty_random_normal(mean, sd, state)
}


##' @export
##' @rdname monty_random_normal
monty_random_n_normal <- function(n_samples, mean, sd, state) {
  cpp_monty_random_n_normal(n_samples, mean, sd, state)
}


##' Sample from a uniform distribution
##'
##' @title Sample from uniform distribution
##'
##' @param min The minimum value of the uniform distribution
##'
##' @param max The maximum value of the uniform distribution
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
monty_random_uniform <- function(min, max, state) {
  cpp_monty_random_uniform(min, max, state)
}


##' @export
##' @rdname monty_random_uniform
monty_random_n_uniform <- function(n_samples, min, max, state) {
  cpp_monty_random_n_uniform(n_samples, min, max, state)
}


##' Sample from a beta-binomial distribution.  There are two
##' parameterisations available one in terms of probability and
##' dispersion and the other in terms of two shape parameters.
##'
##' @title Sample from beta-binomial distribution
##'
##' @param size The number of trials (zero or more)
##'
##' @param prob The mean probability of success on each trial (between
##'   0 and 1)
##'
##' @param rho The dispersion parameter (between 0 and 1)
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @rdname monty_random_beta_binomial
##' @export
monty_random_beta_binomial_prob <- function(size, prob, rho, state) {
  cpp_monty_random_beta_binomial_prob(size, prob, rho, state)
}


##' @export
##' @rdname monty_random_beta_binomial
monty_random_n_beta_binomial_prob <- function(n_samples, size, prob, rho,
                                              state) {
  cpp_monty_random_n_beta_binomial_prob(n_samples, size, prob, rho, state)
}


##' @param a The first shape parameter (zero or more)
##'
##' @param b The second shape parameter (zero or more)
##'
##' @export
##' @rdname monty_random_beta_binomial
monty_random_beta_binomial_ab <- function(size, a, b, state) {
  cpp_monty_random_beta_binomial_ab(size, a, b, state)
}


##' @export
##' @rdname monty_random_beta_binomial
monty_random_n_beta_binomial_ab <- function(n_samples, size, a, b, state) {
  cpp_monty_random_n_beta_binomial_ab(n_samples, size, a, b, state)
}


##' Sample from a hypergeometric distribution.
##'
##' @title Sample from hypergeometric distribution
##'
##' @param n1 The number of white balls in the urn (called n in
##'   R's [rhyper])
##'
##' @param n2 The number of black balls in the urn (called m in
##'   R's [rhyper])
##'
##' @param k The number of balls to draw
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
monty_random_hypergeometric <- function(n1, n2, k, state) {
  cpp_monty_random_hypergeometric(n1, n2, k, state)
}


##' @export
##' @rdname monty_random_hypergeometric
monty_random_n_hypergeometric <- function(n_samples, n1, n2, k, state) {
  cpp_monty_random_n_hypergeometric(n_samples, n1, n2, k, state)
}


##' Sample from a truncated normal distribution
##'
##' @title Sample from truncated normal
##'
##' @param mean The mean of the **parent** (untruncated) normal
##'   distribution (this is not necessarily the mean of the truncated
##'   distribution, unless the `min` and `max` are symmetrically
##'   placed around `mean`)
##'
##' @param sd The standard deviation of the **parent** distribution
##'   (this is not the same as the standard deviation of the truncated
##'   distribution with finite bounds).
##'
##' @param min The lower bound (can be `-Inf`).
##'
##' @param max The upper bound (can be `Inf`).
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
monty_random_truncated_normal <- function(mean, sd, min, max, state) {
  cpp_monty_random_truncated_normal(mean, sd, min, max, state)
}


##' @export
##' @rdname monty_random_truncated_normal
monty_random_n_truncated_normal <- function(n_samples, mean, sd, min, max,
                                            state) {
  cpp_monty_random_n_truncated_normal(n_samples, mean, sd, min, max, state)
}


##' Sample from a Weibull distribution
##'
##' @title Sample from Weibull
##'
##' @param shape Shape
##'
##' @param scale Scale
##'
##' @inheritParams monty_random_real
##' @inherit monty_random_real return
##'
##' @export
monty_random_weibull <- function(mean, sd, min, max, state) {
  cpp_monty_random_weibull(shape, scale, state)
}


##' @export
##' @rdname monty_random_weibull
monty_random_n_weibull <- function(n_samples, shape, scale, state) {
  cpp_monty_random_n_weibull(n_samples, shape, scale, state)
}