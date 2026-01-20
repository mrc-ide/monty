# Get and set random number state

Get and set internal random number state

## Usage

``` r
monty_rng_state(state)

monty_rng_set_state(value, state)
```

## Arguments

- state:

  The random number state, from
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md)

- value:

  A vector of raw values, typically the result of exporting a random
  state with `monty_rng_state()`

## Value

A vector of raws

## Examples

``` r
s1 <- monty_rng_create()
r1 <- monty_rng_state(s1)

s2 <- monty_rng_create(seed = r1)
identical(r1, monty_rng_state(s2))
#> [1] TRUE
monty_random_real(s1)
#> [1] 0.3854807
monty_random_real(s2)
#> [1] 0.3854807

monty_rng_set_state(r1, s1)
monty_random_real(s1)
#> [1] 0.3854807
monty_random_real(s1)
#> [1] 0.05921009
monty_random_real(s2)
#> [1] 0.05921009
```
