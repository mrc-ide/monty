# Run MCMC chains simultaneously

Run chains *simultaneously*. This differs from
[monty_runner_parallel](https://mrc-ide.github.io/monty/reference/monty_runner_parallel.md),
which runs chains individually in parallel by working with models that
can evaluate multiple densities at the same time. There are situations
where this might be faster than running in parallel, but primarily this
exists so that we can see that samplers can work with multiple samples
at once.

## Usage

``` r
monty_runner_simultaneous(progress = NULL)
```

## Arguments

- progress:

  Optional logical, indicating if we should print a progress bar while
  running. If `NULL`, we use the value of the option `monty.progress` if
  set, otherwise we show the progress bar (as it is typically wanted).
  Alternatively, you can provide a string indicating the progress bar
  type. Options are `fancy` (equivalent to `TRUE`), `none` (equivalent
  to `FALSE`) and `simple` (a very simple text-mode progress indicator
  designed play nicely with logging; it does not use special codes to
  clear the line).

## Value

A runner of class `monty_runner` that can be passed to
[`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)

## Examples

``` r
m <- monty_example("banana")
s <- monty_sampler_random_walk(vcv = diag(2) * 0.01)
r <- monty_runner_simultaneous()
samples <- monty_sample(m, s, 200, runner = r)
#> ⡀⠀ Sampling  ■                                |   0% ETA:  1s
#> ✔ Sampled 200 steps across 1 chain in 23ms
#> 
```
