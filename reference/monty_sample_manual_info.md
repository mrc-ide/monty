# Get information about manually scheduled samples

Get information about the status of manually scheduled samples.

## Usage

``` r
monty_sample_manual_info(path)
```

## Arguments

- path:

  The path used in the call to
  [monty_sample_manual_prepare](https://mrc-ide.github.io/monty/reference/monty_sample_manual_prepare.md)

## Value

Invisibly, a logical vector, `TRUE` for completed chains and `FALSE` for
incomplete chains.

## Examples

``` r
model <- monty_example("banana")
sampler <- monty_sampler_random_walk(vcv = diag(2) * 0.05)
path <- tempfile()
monty_sample_manual_prepare(model, sampler, 100, path)
monty_sample_manual_info(path)
#> 
#> ── Manual monty sampling at /tmp/Rtmp4eQ69u/file1bc770f92e07 ───────────────────
#> ℹ Created 2026-01-20 11:51:04
#> ℹ 100 steps x 1 chains
#> ✖ No chains complete

# Run the (single) chain
monty_sample_manual_run(1, path)
#> ⡀⠀ Sampling  ■                                |   1% ETA:  0s
#> ✔ Sampled 100 steps across 1 chain in 20ms
#> 
monty_sample_manual_info(path)
#> 
#> ── Manual monty sampling at /tmp/Rtmp4eQ69u/file1bc770f92e07 ───────────────────
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
