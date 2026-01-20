# Sample from Poisson distribution

Sample from the Poisson distribution

## Usage

``` r
monty_random_poisson(lambda, state)

monty_random_n_poisson(n_samples, lambda, state)
```

## Arguments

- lambda:

  The mean (zero or more, length 1 or n). Only valid for lambda \<= 10^7

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
