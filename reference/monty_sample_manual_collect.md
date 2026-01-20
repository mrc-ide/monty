# Collect manually run samples

Collect samples from chains that have been run with
[monty_sample_manual_prepare](https://mrc-ide.github.io/monty/reference/monty_sample_manual_prepare.md)
and
[monty_sample_manual_run](https://mrc-ide.github.io/monty/reference/monty_sample_manual_run.md).
If any chain has not completed, we will error.

## Usage

``` r
monty_sample_manual_collect(
  path,
  samples = NULL,
  restartable = FALSE,
  append = TRUE
)
```

## Arguments

- path:

  The path used in the call to
  [monty_sample_manual_prepare](https://mrc-ide.github.io/monty/reference/monty_sample_manual_prepare.md)

- samples:

  Samples from the parent run. You need to provide these where
  `save_samples` was set to anything other than "value"

- restartable:

  Logical, indicating if the chains should be restartable. This will add
  additional data to the chains object. Note that this is controlled at
  chain collection and not creation.

- append:

  Logical, indicating if we should append the results of the resumed
  chain together with the original chain.

## Value

A `monty_samples` object.

## Examples

``` r
model <- monty_example("banana")
sampler <- monty_sampler_random_walk(vcv = diag(2) * 0.05)
path <- tempfile()
monty_sample_manual_prepare(model, sampler, 100, path)
monty_sample_manual_info(path)
#> 
#> ── Manual monty sampling at /tmp/Rtmp4eQ69u/file1bc7610116ad ───────────────────
#> ℹ Created 2026-01-20 11:51:04
#> ℹ 100 steps x 1 chains
#> ✖ No chains complete

# Run the (single) chain
monty_sample_manual_run(1, path)
monty_sample_manual_info(path)
#> 
#> ── Manual monty sampling at /tmp/Rtmp4eQ69u/file1bc7610116ad ───────────────────
#> ℹ Created 2026-01-20 11:51:04
#> ℹ 100 steps x 1 chains
#> ✔ All chains complete

# Collect the results
monty_sample_manual_collect(path)
#> 
#> ── <monty_samples: 2 parameters x 100 samples x 1 chain> ───────────────────────
#> ℹ Parameters: 'alpha' and 'beta'
#> ℹ Conversion to other types is possible:
#> → ✔ posterior::as_draws_array() [package loaded]
#> → ✔ posterior::as_draws_df() [package loaded]
#> → ✔ coda::as.mcmc.list() [package loaded]
#> ℹ See `?monty_sample()` and `vignette("samples")` for more information

# Clean up samples
monty_sample_manual_cleanup(path)
```
