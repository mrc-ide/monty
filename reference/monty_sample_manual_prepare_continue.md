# Prepare to continue sampling with manual scheduling

Prepare to continue sampling from a model, with manual chain
orchestration. This function is to
[monty_sample_continue](https://mrc-ide.github.io/monty/reference/monty_sample_continue.md)
what
[monty_sample_manual_prepare](https://mrc-ide.github.io/monty/reference/monty_sample_manual_prepare.md)
is to
[monty_sample](https://mrc-ide.github.io/monty/reference/monty_sample.md).
The original set of samples do not need to have been run manually.

## Usage

``` r
monty_sample_manual_prepare_continue(
  samples,
  n_steps,
  path,
  save_samples = "hash"
)
```

## Arguments

- samples:

  A `monty_samples` object created by
  [`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)

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

- save_samples:

  Control over saving samples into the inputs. The choices here are
  `hash` (the default) where we save a hash and validate that in
  [monty_sample_manual_collect](https://mrc-ide.github.io/monty/reference/monty_sample_manual_collect.md),
  `value` where the samples are themselves saved and you can omit the
  `samples ` argument to
  [monty_sample_manual_collect](https://mrc-ide.github.io/monty/reference/monty_sample_manual_collect.md),
  or `nothing`, in which we save nothing, and you just have to get it
  right.

## Value

Invisibly, the path used to store files (the same as the value of the
`path` argument)
