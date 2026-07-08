# Sample from negative binomial distribution

Sample from a negative binomial distribution. There are two
parameterisations available: one in based on the success probability and
the other based on the mean.

## Usage

``` r
monty_random_negative_binomial_prob(size, prob, state)

monty_random_n_negative_binomial_prob(n_samples, size, prob, state)

monty_random_negative_binomial_mu(size, mu, state)

monty_random_n_negative_binomial_mu(n_samples, size, mu, state)
```

## Arguments

- size:

  The target number of successful trials (zero or more)

- prob:

  The probability of success on each trial (between 0 and 1)

- state:

  The random number state, from
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md)

- n_samples:

  The number of samples to take, **per stream**. When using the
  multiple-sample interface, all other parameters are held constant (per
  stream).

- mu:

  The mean (zero or more)

## Value

A vector of random numbers, the same length as the number of streams in
`state`.
