# Flatten chains

Flatten chains in the results of running
[`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md),
wherein `pars`, `density` and typically objects in `observations` will
have the last two dimensions representing 'samples' and 'chains'.
Flattening the chains results in the chains dimension being collapsed
into the samples dimension.

## Usage

``` r
monty_flatten_chains(samples)
```

## Arguments

- samples:

  A `monty_samples` object, from running
  [`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)

## Value

A `monty_samples` object with the chains dimension collapsed into the
samples dimension.
