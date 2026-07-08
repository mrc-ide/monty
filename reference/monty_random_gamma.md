# Sample from a gamma distribution. There are two parameterisations here, one in terms of rate, and one in terms of scale.

Sample from gamma distribution

## Usage

``` r
monty_random_gamma_scale(shape, scale, state)

monty_random_n_gamma_scale(n_samples, shape, scale, state)

monty_random_gamma_rate(shape, rate, state)

monty_random_n_gamma_rate(n_samples, shape, rate, state)
```

## Arguments

- shape:

  Shape

- scale:

  Scale '

- state:

  The random number state, from
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md)

- n_samples:

  The number of samples to take, **per stream**. When using the
  multiple-sample interface, all other parameters are held constant (per
  stream).

- rate:

  Rate

## Value

A vector of random numbers, the same length as the number of streams in
`state`.
