# Run MCMC chain in parallel

Run MCMC chains in parallel (at the same time). This runner uses the
`parallel` package to distribute your chains over a number of worker
processes on the same machine. Compared with
[monty_runner_callr](https://mrc-ide.github.io/monty/reference/monty_runner_callr.md)
(Which is similar to the "worker" support in `mcstate` version 1), this
is very simple. In particular we do not report back any information
about progress while a chain is running on a worker or even across
chains. There's also no support to warn you if your number of chains do
not neatly divide through by the number of workers. Mostly this exists
as a proof of concept for us to think about the different interfaces.
Unless your chains are quite slow, the parallel runner will be slower
than the serial runner
([monty_runner_serial](https://mrc-ide.github.io/monty/reference/monty_runner_serial.md))
due to the overhead cost of starting the cluster.

## Usage

``` r
monty_runner_parallel(n_workers)
```

## Arguments

- n_workers:

  Number of workers to create a cluster from. In a multi-user setting be
  careful not to set this to more cores than you are allowed to use. You
  can use
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  to get an estimate of the number of cores you have on a single user
  system (but this is often an overestimate as it returns the number of
  logical cores, including those from "hyperthreading"). Fewer cores
  than this will be used if you run fewer chains than you have workers.

## Value

A runner of class `monty_runner` that can be passed to
[`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)
