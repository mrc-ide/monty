# Unflatten chains

Unflatten chains in `monty_samples` object that has previously had the
chains flattened with
[`monty_flatten_chains()`](https://mrc-ide.github.io/monty/reference/monty_flatten_chains.md),
reversing the effects of that function.

## Usage

``` r
monty_unflatten_chains(samples)
```

## Arguments

- samples:

  A `monty_samples` object, that has been run through
  [`monty_flatten_chains()`](https://mrc-ide.github.io/monty/reference/monty_flatten_chains.md)

## Value

A `monty_samples` object with the chains dimension restored.
