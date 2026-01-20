# Sample from exponential distribution

Sample from an exponential distribution. There are two parameterisations
here, one in terms of the rate of the exponential, and one in terms of
the mean (or scale).

## Usage

``` r
monty_random_exponential_rate(rate, state)

monty_random_n_exponential_rate(n_samples, rate, state)

monty_random_exponential_mean(mean, state)

monty_random_n_exponential_mean(n_samples, mean, state)
```

## Arguments

- rate:

  The rate of the exponential

- state:

  The random number state, from
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md)

- n_samples:

  The number of samples to take, **per stream**. When using the
  multiple-sample interface, all other parameters are held constant (per
  stream).

- mean:

  The mean of the exponential distribution (i.e., `1 / rate`)

## Value

A vector of random numbers, the same length as the number of streams in
`state`.

## Examples

``` r
state <- monty_rng_create()
monty_random_exponential_rate(0.2, state)
#> [1] 0.9718111
summary(monty_random_n_exponential_rate(2000, 0.2, state))
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#>  0.000614  1.327562  3.406649  4.963110  6.957489 35.318274 
```
