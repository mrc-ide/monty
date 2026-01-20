# Run MCMC chain in series

Run MCMC chains in series (one after another). This is the simplest
chain runner, and the default used by
[`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md).
It has nothing that can be configured (yet).

## Usage

``` r
monty_runner_serial(progress = NULL)
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
