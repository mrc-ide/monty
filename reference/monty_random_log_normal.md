# Sample from log-normal

Sample from a log-normal distribution

## Usage

``` r
monty_random_log_normal(meanlog, sdlog, state)

monty_random_n_log_normal(n_samples, meanlog, sdlog, state)
```

## Arguments

- meanlog:

  The mean of the distribution on the log scale

- sdlog:

  The standard deviation of the distribution on the log scale

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
