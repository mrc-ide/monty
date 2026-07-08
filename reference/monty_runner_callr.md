# Run MCMC chains in parallel with `callr`

Run MCMC chains in parallel (at the same time). This runner uses the
`callr` package to distribute your chains over a number of worker
processes on the same machine. If you have used `mcstate`, this is the
same as "worker" processes. Unless your chains take a few seconds to
run, this will be slower than running with the default serial runner
([monty_runner_serial](https://mrc-ide.github.io/monty/reference/monty_runner_serial.md)),
however for long running chains, the speed-up will typically scale with
workers added, so long as your chains divide neatly over workers.

## Usage

``` r
monty_runner_callr(n_workers, progress = NULL)
```

## Arguments

- n_workers:

  The number of workers to use. This should be no larger than the number
  of chains (though this is harmless) and no larger than the total
  number of cores available on your computer. Ideally the number of
  chains you want to run is a multiple of this number (for example, if
  you had 8 chains, then 1, 2, 4, and 8 are good choices of `n_workers`,
  and 7 workers will likely be no faster than 4).

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
