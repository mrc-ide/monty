# Sample from zero-inflated negative binomial distribution

Sample from a zero-inflated negative binomial distribution. With
probability `pi0` a 0 is drawn, otherwise it draws from the
corresponding standard negative binomial distribution (which may also
result in a 0 being drawn). As with the standard negative binomial
distribution, there are two parameterisations available, based on either
the success probability or mean of the negative binomial part of the
distribution.

## Usage

``` r
monty_random_zi_negative_binomial_prob(pi0, size, prob, state)

monty_random_n_zi_negative_binomial_prob(n_samples, pi0, size, prob, state)

monty_random_zi_negative_binomial_mu(pi0, size, mu, state)

monty_random_n_zi_negative_binomial_mu(n_samples, pi0, size, mu, state)
```

## Arguments

- pi0:

  The probability of excess zeros

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

  The mean (zero or more) of the negative binomial part of the
  distribution. Note the mean of the zero-inflated negative binomial
  distribution will actually be `(1 - pi0) * mu`

## Value

A vector of random numbers, the same length as the number of streams in
`state`.
