# Continue sampling

Continue (restart) chains started by
[monty_sample](https://mrc-ide.github.io/monty/reference/monty_sample.md).
Requires that the original chains were run with `restartable = TRUE`.
Running chains this way will result in the final state being exactly the
same as running for the total (original + continued) number of steps in
a single push.

## Usage

``` r
monty_sample_continue(
  samples,
  n_steps,
  restartable = FALSE,
  runner = NULL,
  append = TRUE
)
```

## Arguments

- samples:

  A `monty_samples` object created by
  [`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)

- n_steps:

  The number of new steps to run

- restartable:

  Logical, indicating if the chains should be restartable. This will add
  additional data to the chains object.

- runner:

  Optionally, a runner for your chains. The default is to continue with
  the backend that you used to start the chains via
  [monty_sample](https://mrc-ide.github.io/monty/reference/monty_sample.md)
  (or on the previous restart with this function). You can use this
  argument to change the runner, which might be useful if transferring a
  pilot run from a high-resource environment to a lower-resource
  environment. If given, must be a `monty_runner` object such as
  [monty_runner_serial](https://mrc-ide.github.io/monty/reference/monty_runner_serial.md)
  or
  [monty_runner_parallel](https://mrc-ide.github.io/monty/reference/monty_runner_parallel.md).
  You can use this argument to change the configuration of a runner, as
  well as the type of runner (e.g., changing the number of allocated
  cores).

- append:

  Logical, indicating if we should append the results of the resumed
  chain together with the original chain.

## Value

A list of parameters and densities
