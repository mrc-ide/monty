# Run sample with manual scheduling

Run a chain that was prepared using
[monty_sample_manual_prepare](https://mrc-ide.github.io/monty/reference/monty_sample_manual_prepare.md).

## Usage

``` r
monty_sample_manual_run(chain_id, path, progress = NULL)
```

## Arguments

- chain_id:

  The id for the chain to run, an integer. If you provide an integer
  that does not correspond to a chain in 1 to `n_chains` (where
  `n_chains` was the argument passed to `monty_sample_manual_prepare` it
  is an error.

- path:

  The path used in the call to
  [monty_sample_manual_prepare](https://mrc-ide.github.io/monty/reference/monty_sample_manual_prepare.md)

- progress:

  Optional logical, indicating if we should print a progress bar while
  running. If `NULL`, we use the value of the option `monty.progress` if
  set, otherwise we show the progress bar (as it is typically wanted).
  Alternatively, you can provide a string indicating the progress bar
  type. Options are `fancy` (equivalent to `TRUE`), `none` (equivalent
  to `FALSE`) and `simple` (a very simple text-mode progress indicator
  designed play nicely with logging; it does not use special codes to
  clear the line).

## Warning:

There is no lock mechanism; you can start a single chain many times.
Don't do this.

## Examples

``` r
model <- monty_example("banana")
sampler <- monty_sampler_random_walk(vcv = diag(2) * 0.05)
path <- tempfile()
monty_sample_manual_prepare(model, sampler, 100, path)
monty_sample_manual_info(path)
#> 
#> ── Manual monty sampling at /tmp/Rtmp4eQ69u/file1bc736ad5e3a ───────────────────
#> ℹ Created 2026-01-20 11:51:05
#> ℹ 100 steps x 1 chains
#> ✖ No chains complete

# Run the (single) chain
monty_sample_manual_run(1, path)
monty_sample_manual_info(path)
#> 
#> ── Manual monty sampling at /tmp/Rtmp4eQ69u/file1bc736ad5e3a ───────────────────
#> ℹ Created 2026-01-20 11:51:05
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
