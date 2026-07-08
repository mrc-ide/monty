# Sample from truncated normal

Sample from a truncated normal distribution

## Usage

``` r
monty_random_truncated_normal(mean, sd, min, max, state)

monty_random_n_truncated_normal(n_samples, mean, sd, min, max, state)
```

## Arguments

- mean:

  The mean of the **parent** (untruncated) normal distribution (this is
  not necessarily the mean of the truncated distribution, unless the
  `min` and `max` are symmetrically placed around `mean`)

- sd:

  The standard deviation of the **parent** distribution (this is not the
  same as the standard deviation of the truncated distribution with
  finite bounds).

- min:

  The lower bound (can be `-Inf`).

- max:

  The upper bound (can be `Inf`).

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
