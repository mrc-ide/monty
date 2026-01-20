# Sample from binomial distribution

Sample from the binomial distribution

## Usage

``` r
monty_random_binomial(size, prob, state)

monty_random_n_binomial(n_samples, size, prob, state)
```

## Arguments

- size:

  The number of trials

- prob:

  The probability of success on each trial

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

## Examples

``` r
state <- monty_rng_create()
monty_random_binomial(10, 0.3, state)
#> [1] 3
table(monty_random_n_binomial(2000, 10, 0.3, state))
#> 
#>   0   1   2   3   4   5   6   7   8 
#>  58 264 445 524 369 244  66  25   5 
```
