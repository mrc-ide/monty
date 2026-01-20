# Sample from Uniform(0, 1)

Generate a random number uniformly sampled on the range 0 to 1; this is
the most basic of all random number functions in monty and all other
algorithms are composed from this. Quite often, you will want a number
on \$\[0, 1\]\$ (e.g., for a Bernoulli trial), and this function is the
most efficient way of generating one.

## Usage

``` r
monty_random_real(state)

monty_random_n_real(n_samples, state)
```

## Arguments

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
monty_random_real(state)
#> [1] 0.6498534
monty_random_n_real(5, state)
#> [1] 0.7836332 0.6252441 0.4862286 0.8272822 0.2704858
```
