# Sample from Weibull

Sample from a Weibull distribution

## Usage

``` r
monty_random_weibull(shape, scale, state)

monty_random_n_weibull(n_samples, shape, scale, state)
```

## Arguments

- shape:

  Shape

- scale:

  Scale

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
