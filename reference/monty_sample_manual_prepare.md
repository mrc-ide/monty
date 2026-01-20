# Prepare to sample with manual scheduling

Run an MCMC, but schedule execution of the chains yourself. Use this if
you want to distribute chains over (say) the nodes of an HPC system. The
arguments are the same as for
[monty_sample](https://mrc-ide.github.io/monty/reference/monty_sample.md),
except that the `runner` argument is missing as you will be looking
after that yourself. After using this function, you will generally be
wanting to run
[monty_sample_manual_run](https://mrc-ide.github.io/monty/reference/monty_sample_manual_run.md)
and
[monty_sample_manual_collect](https://mrc-ide.github.io/monty/reference/monty_sample_manual_collect.md).

## Usage

``` r
monty_sample_manual_prepare(
  model,
  sampler,
  n_steps,
  path,
  initial = NULL,
  n_chains = 1L,
  burnin = NULL,
  thinning_factor = NULL
)
```

## Arguments

- model:

  The model to sample from; this should be a `monty_model` for now, but
  we might change this in future to test to see if things match an
  interface rather than a particular class attribute.

- sampler:

  A sampler to use. These will be described later, but we hope to make
  these reasonably easy to implement so that we can try out different
  sampling ideas. For now, the only sampler implemented is
  [`monty_sampler_random_walk()`](https://mrc-ide.github.io/monty/reference/monty_sampler_random_walk.md).

- n_steps:

  The number of steps to run the sampler for.

- path:

  The path to write inputs and outputs to. This should be a path to a
  directory which does not yet exist, or which is empty; we will create
  one here. The contents of this directory are managed by `monty` and
  the names and contents of files here are an implementation detail and
  should not be relied on. Calling
  [`monty_sample_manual_cleanup()`](https://mrc-ide.github.io/monty/reference/monty_sample_manual_cleanup.md)
  will delete the directory in its entirety. Be aware that if you use
  [`tempfile()`](https://rdrr.io/r/base/tempfile.html) here (which can
  be a reasonable choice!) that this path will be deleted when your R
  process ends, so if using these the process calling
  `monty_sample_manual_prepare` should outlive running all sampling.

- initial:

  Optionally, initial parameter values for the sampling. If not given,
  we sample from the model (or its prior). Alternatively, you can
  provide a `monty_samples` object here – the result of a previous call
  to this function – and we will sample some starting points from the
  final portion of the chains (the exact details here are subject to
  change, but we'll sample from the last 20 points or 5% of the chain,
  which ever smaller, with replacement, pooled across all chains in the
  previous sample).

- n_chains:

  Number of chains to run. The default is to run a single chain, but you
  will likely want to run more.

- burnin:

  Number of steps to discard as burnin. This affects only the recording
  of steps as your chains run; we don't record the first `burnin` steps.
  Generally you would want to do this in post-processing with
  [`monty_samples_thin()`](https://mrc-ide.github.io/monty/reference/monty_samples_thin.md)
  as this data is discarded with no chance of getting it back. However,
  if your observation process creates a large amount of data, then you
  may prefer to apply a burnin here to reduce how much memory is used.

- thinning_factor:

  A thinning factor to apply while the chain is running. If given, then
  we save every `thinning_factor`'th step. So if `thinning_factor = 2`
  we save every second step, and if 10, we'd save every 10th. Like
  `burnin` above, it is preferable to apply this in post processing with
  [`monty_samples_thin()`](https://mrc-ide.github.io/monty/reference/monty_samples_thin.md).
  However, for slow-mixing chains that have a large observer output you
  can use this to reduce the memory usage. Use of `thinning_factor`
  requires that `n_steps` is an even multiple of `thinning_factor`; so
  if `thinning_factor` is 10, then `n_steps` must be a multiple of 10.
  This ensures that the last step is in the sample. The thinning factor
  cannot be changed when continuing a chain.

## Value

Invisibly, the path used to store files (the same as the value of the
`path` argument)

## Details

In contrast to
[monty_sample](https://mrc-ide.github.io/monty/reference/monty_sample.md)
there is no `runner` argument here, because by using this function
directly you are taking responsibility for being your own runner.

As with the ways of running a set of chains in monty, it is expected
that using `monty_sample_manual_*` will result in the same samples being
generated as if you had used `monty_sample` with a runner of your
choice.

## See also

[monty_sample_manual_run](https://mrc-ide.github.io/monty/reference/monty_sample_manual_run.md)
to run the chains and
[monty_sample_manual_collect](https://mrc-ide.github.io/monty/reference/monty_sample_manual_collect.md)
/
[monty_sample_manual_cleanup](https://mrc-ide.github.io/monty/reference/monty_sample_manual_cleanup.md)
to collect results and clean up.
[monty_sample_manual_info](https://mrc-ide.github.io/monty/reference/monty_sample_manual_info.md)
can print human-readable information about the state of a manual run.

## Examples

``` r
model <- monty_example("banana")
sampler <- monty_sampler_random_walk(vcv = diag(2) * 0.05)
path <- tempfile()
monty_sample_manual_prepare(model, sampler, 100, path)
monty_sample_manual_info(path)
#> 
#> ── Manual monty sampling at /tmp/Rtmp4eQ69u/file1bc761c795a6 ───────────────────
#> ℹ Created 2026-01-20 11:51:04
#> ℹ 100 steps x 1 chains
#> ✖ No chains complete

# Run the (single) chain
monty_sample_manual_run(1, path)
monty_sample_manual_info(path)
#> 
#> ── Manual monty sampling at /tmp/Rtmp4eQ69u/file1bc761c795a6 ───────────────────
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
