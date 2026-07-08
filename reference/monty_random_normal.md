# Sample from normal distribution

Sample from a normal distribution

## Usage

``` r
monty_random_normal(mean, sd, state, algorithm = "box_muller")

monty_random_n_normal(n_samples, mean, sd, state, algorithm = "box_muller")
```

## Arguments

- mean:

  The mean of the normal distribution

- sd:

  The standard deviation of the normal distribution

- state:

  The random number state, from
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md)

- algorithm:

  The algorithm to use for the normal samples; currently `box_muller`,
  `polar` and and `ziggurat` are supported, with the latter being
  considerably faster. The default may change in a future version.

- n_samples:

  The number of samples to take, **per stream**. When using the
  multiple-sample interface, all other parameters are held constant (per
  stream).

## Value

A vector of random numbers, the same length as the number of streams in
`state`.
