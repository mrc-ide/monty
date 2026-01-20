# Sample from beta-binomial distribution

Sample from a beta-binomial distribution. There are two
parameterisations available: one in terms of probability and dispersion
and the other in terms of two shape parameters.

## Usage

``` r
monty_random_beta_binomial_prob(size, prob, rho, state)

monty_random_n_beta_binomial_prob(n_samples, size, prob, rho, state)

monty_random_beta_binomial_ab(size, a, b, state)

monty_random_n_beta_binomial_ab(n_samples, size, a, b, state)
```

## Arguments

- size:

  The number of trials (zero or more)

- prob:

  The mean probability of success on each trial (between 0 and 1)

- rho:

  The dispersion parameter (between 0 and 1)

- state:

  The random number state, from
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md)

- n_samples:

  The number of samples to take, **per stream**. When using the
  multiple-sample interface, all other parameters are held constant (per
  stream).

- a:

  The first shape parameter (zero or more)

- b:

  The second shape parameter (zero or more)

## Value

A vector of random numbers, the same length as the number of streams in
`state`.
