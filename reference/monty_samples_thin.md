# Thin samples

Thin results of running
[`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md),
reducing autocorrelation between samples and saving space. This function
may be useful before running onward simulations, or before saving output
to disk.

## Usage

``` r
monty_samples_thin(samples, thinning_factor = NULL, burnin = NULL)
```

## Arguments

- samples:

  A `monty_samples` object, from running
  [`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)

- thinning_factor:

  Optional integer thinning factor. If given, then we save every
  `thinning_factor`'th step. So if `thinning_factor = 2` we save every
  second step, and if 10, we'd save every 10th. We will always include
  the last point in the chain, and exclude points counting backwards.

- burnin:

  Number of steps to discard as burnin from the start of the chain.

## Value

A `monty_samples` object (as for
[`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)),
typically with fewer samples.

## Limitations

Subsetting parameters (`$pars`) and density (`$density`) is easy enough,
but the main use of this function is subsetting chains that have
observations, otherwise you could simply cast to `samples_df` and use
functions from the `posterior` package.

We can only subset observations where the observer was able to tidy them
up into a nice array for us. This will typically be the case (for
example when using odin/dust, trajectories will be in a nice array).

More specifically, an array is "nice" if the last two dimensions
represent samples and chains; in that case we subset along the samples
dimension and leave everything else alone. For each element of
`$observations` that cannot be subsetted, we will issue a warning.

We cannot generally subset "details", and will pass that along
unmodified.
