# Sample from hypergeometric distribution

Sample from a hypergeometric distribution.

## Usage

``` r
monty_random_hypergeometric(n1, n2, k, state)

monty_random_n_hypergeometric(n_samples, n1, n2, k, state)
```

## Arguments

- n1:

  The number of white balls in the urn (called n in R's
  [rhyper](https://rdrr.io/r/stats/Hypergeometric.html))

- n2:

  The number of black balls in the urn (called m in R's
  [rhyper](https://rdrr.io/r/stats/Hypergeometric.html))

- k:

  The number of balls to draw

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
