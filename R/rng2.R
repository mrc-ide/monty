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
  preserve_particle_dimension <- preserve_stream_dimension || n_streams > 1
  ptr <- monty_rng_alloc(seed, n_streams, deterministic)
  ret <- list(ptr = ptr,
              n_streams = n_streams,
              n_threads = n_threads,
              deterministic = deterministic,
              preserve_stream_dimension = preserve_stream_dimension)
  class(ret) <- "monty_random_state"
  ret
}

##' Generate a random number uniformly sampled on the range 0 to 1;
##' this is the most basic of all random number functions in monty and
##' all other algorithms are composed from this.  Quite often, you
##' will want a number on [0, 1] (e.g., for a Bernoulli trial), and
##' this function is the most efficient way of generating one (though
##' the difference between this and [monty_random_uniform()] with `min
##' = 0` and `max = 1` is very marginal, especially when called from
##' R).
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
  cpp_monty_random_real(state$ptr)
}


##' @param n_samples The number of samples to take, **per stream**.  When this is
##' @export
##' @rdname monty_random_real
monty_random_n_real <- function(n_samples, state) {
  cpp_monty_random_real(n_samples, state)
}


##' Sample from the binomial distribution
##'
##' @title Sample from binomial distribution
##'
##' @param size The number of trials
##'
##' @param prob The probability of success on each trial
##'
##' @param state
##' @return
##' @author Rich FitzJohn
monty_random_binomial <- function(size, prob, state) {
  cpp_monty_random_binomial(size, prob, state)
}


monty_random_exponential_rate <- function(rate, state) {
  cpp_monty_random_exponential_rate(rate, state)
}
