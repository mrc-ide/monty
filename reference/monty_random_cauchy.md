# Sample from Cauchy distribution

Sample from the Cauchy distribution

## Usage

``` r
monty_random_cauchy(location, scale, state)

monty_random_n_cauchy(n_samples, location, scale, state)
```

## Arguments

- location:

  Location of the distribution (the same as the median and mode)

- scale:

  A scale parameter which specifies the half-width at half-maximum
  (HWHM)

- state:

  The random number state, from
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md)

- n_samples:

  The number of samples to take, **per stream**. When using the
  multiple-sample interface, all other parameters are held constant (per
  stream).

## Value

A vector of random numbers, the same length as the number of streams in
`state`.
