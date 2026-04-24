# Sample from zero-inflated Poisson distribution

Sample from the zero-inflated Poisson distribution. With probability
`pi0` a 0 is drawn, otherwise it draws from the corresponding standard
Poisson distribution (which may also result in a 0 being drawn).

## Usage

``` r
monty_random_zi_poisson(pi0, lambda, state)

monty_random_n_zi_poisson(n_samples, pi0, lambda, state)
```

## Arguments

- pi0:

  The probability of extra zeros

- lambda:

  The mean (zero or more, length 1 or n) of the Poisson part of the
  distribution. Only valid for lambda \<= 10^7. Note that the mean of
  the zero-inflated Poisson distribution will actually be
  `(1 - pi0) * lambda`

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
