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
##' @return An object of class `monty_random_state`, which can be
##'   passed as the `state` argument to random-number producing
##'   functions, such as [monty_random_real]
##'
##' @export
##' @examples
##' state <- monty_random_create()
##' state
##'
##' monty_random_real(state)
monty_random_create <- function(n_streams = 1L, seed = NULL,
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
  class(ptr) <- "monty_random_state"
  ptr
}


##' Get and set internal random number state
##'
##' @title Get and set random number state
##'
##' @param state The random number state, from [monty_random_create]
##'
##' @return A vector of raws
##' @export
##' @examples
##' s1 <- monty_random_create()
##' r1 <- monty_random_state(s1)
##'
##' s2 <- monty_random_create(seed = r1)
##' identical(r1, monty_random_state(s2))
##' monty_random_real(s1)
##' monty_random_real(s2)
##'
##' monty_random_set_state(r1, s1)
##' monty_random_real(s1)
##' monty_random_real(s1)
##' monty_random_real(s2)
monty_random_state <- function(state) {
  cpp_monty_random_state(state)
}


##' @param value A vector of raw values, typically the result of
##'   exporting a random state with `monty_random_state()`
##'
##' @export
##' @rdname monty_random_state
monty_random_set_state <- function(value, state) {
  cpp_monty_random_set_state(state, value)
}


##' @export
print.monty_random_state <- function(x, ...) {
  cli::cli_h1("<monty_random_state>")
  cli::cli_li("{attr(x, 'n_streams')} random number stream{?s}")
  cli::cli_li("{attr(x, 'n_threads')} execution thread{?s}")
  invisible(x)
}


##' Generate a random number uniformly sampled on the range 0 to 1;
##' this is the most basic of all random number functions in monty and
##' all other algorithms are composed from this.  Quite often, you
##' will want a number on $\[0, 1\]$ (e.g., for a Bernoulli trial), and
##' this function is the most efficient way of generating one.
##'
##' @title Sample from Uniform(0, 1)
##'
##' @param state The random number state, from [monty_random_create]
##'
##' @return A vector of random numbers, the same length as the number
##'   of streams in `state`.
##'
##' @export
##' @examples
##' state <- monty_random_create()
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
##' state <- monty_random_create()
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
##' state <- monty_random_create()
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
