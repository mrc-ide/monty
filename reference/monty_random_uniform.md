# Sample from uniform distribution

Sample from a uniform distribution

## Usage

``` r
monty_random_uniform(min, max, state)

monty_random_n_uniform(n_samples, min, max, state)
```

## Arguments

- min:

  The minimum value of the uniform distribution

- max:

  The maximum value of the uniform distribution

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
