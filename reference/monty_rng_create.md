# Create a monty random number generator

Create a monty random number generator. This allows you to sample random
numbers from the same random number algorithms as monty provides via C++
to [dust2](https://mrc-ide.github.io/dust2), and which it uses within
its samplers and filters. This function creates the internal state, and
will be passed to actual generation functions `monty_random_*`, such as
[`monty_random_real()`](https://mrc-ide.github.io/monty/reference/monty_random_real.md).

## Usage

``` r
monty_rng_create(
  n_streams = 1L,
  seed = NULL,
  n_threads = 1L,
  deterministic = FALSE,
  preserve_stream_dimension = FALSE
)
```

## Arguments

- n_streams:

  The number of streams to create (see Details)

- seed:

  The initial seed of the random number generator. This can be `NULL`,
  in which case we seed the generator from R's random number state
  (meaning that we respond to `set.seed` as one would expect).
  Alternatively, you can provide an integer here, but this should be
  used sparingly and primarily for testing.

- n_threads:

  The number of threads to use, if OpenMP is enabled.

- deterministic:

  Logical, indicating if we should use "deterministic" mode where
  distributions return their expectations and the state is never
  changed.

- preserve_stream_dimension:

  Logical, indicating if the stream dimension should be preserved in the
  case where `n_streams` is 1 and the multiple-sample functions are
  used. Set this to `TRUE` to ensure that the rank of the result does
  not change with the number of streams (see Details).

## Value

An object of class `monty_rng_state`, which can be passed as the `state`
argument to random-number producing functions, such as
[monty_random_real](https://mrc-ide.github.io/monty/reference/monty_random_real.md)

## Details

Monty's random number generation is very different to that in base R. We
have the concept of "streams" of random numbers, with a generator having
1 or many streams. Each stream is statistically independent, and can be
sampled from simultaneously. When you use any of the random number
functions from R, you will draw one number *per stream*.

Because the random number state can have multiple streams, and we return
a vector, we have a separate set of functions where multiple numbers are
requested *per stream*; these are all prefixed `monty_random_n_` (e.g.,
[`monty_random_n_real()`](https://mrc-ide.github.io/monty/reference/monty_random_real.md)).
These will return a matrix where you have used multiple streams, with
each column representing a stream. If you have a single stream and you
set `preserve_stream_dimension` to `FALSE` then we will drop this
dimension and return a matrix.

## Examples

``` r
state <- monty_rng_create()
state
#> 
#> ── <monty_rng_state> ───────────────────────────────────────────────────────────
#> • 1 random number stream
#> • 1 execution thread

monty_random_real(state)
#> [1] 0.05586274
```
