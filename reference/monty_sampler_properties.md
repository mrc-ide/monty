# Describe sampler properties

Describe properties of a sampler. This is used from
[monty_sampler](https://mrc-ide.github.io/monty/reference/monty_sampler.md)
to advertise what your sampler does about state, what it requires from
the runner and from the model, so that monty can ensure that it is only
used where it is appropriate.

## Usage

``` r
monty_sampler_properties(
  has_state = NULL,
  restartable = NULL,
  allow_multiple_parameters = FALSE,
  requires_gradient = FALSE,
  requires_allow_multiple_parameters = FALSE,
  requires_deterministic = FALSE
)
```

## Arguments

- has_state:

  Optional logical, indicating if the sampler has state. This is
  optional because presence of the state function `state_dump` implies
  this.

- restartable:

  Optional logical, indicating if your sampler can be restarted. If
  `FALSE`, then users cannot use `restartable = TRUE` from
  [`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)
  (and therefore cannot use `monty_continue`). This is optional because
  the presence of the state function `state_restore` implies this.

- allow_multiple_parameters:

  Logical, indicating if your sampler can accept a matrix of parameters
  in order to run multiple chains at once (e.g., with the
  [monty_runner_simultaneous](https://mrc-ide.github.io/monty/reference/monty_runner_simultaneous.md)
  runner, or as part of a parallel tempering scheme with
  [monty_sampler_parallel_tempering](https://mrc-ide.github.io/monty/reference/monty_sampler_parallel_tempering.md)).

- requires_gradient:

  Logical, indicating if the model must provide a gradient in order to
  be used with this sampler.

- requires_allow_multiple_parameters:

  Logical, indicating if the model must be able to accept multiple
  parameters. This is different to `allow_multiple_parameters`, which
  concerns if the *sampler* is able to process multiple parameter sets
  at once. For example,
  [monty_sampler_parallel_tempering](https://mrc-ide.github.io/monty/reference/monty_sampler_parallel_tempering.md)
  sets `allow_multiple_parameters` to `FALSE` but
  `requires_allow_multiple_parameters` to `TRUE`, while
  [monty_sampler_random_walk](https://mrc-ide.github.io/monty/reference/monty_sampler_random_walk.md)
  sets the opposite!

- requires_deterministic:

  Logical, indicating if the model must be deterministic in order to be
  used with this sampler.

## Value

A `monty_sampler_properties` object, which should not be modified.

## Examples

``` r
monty_sampler_properties()
#> 
#> ── <monty_sampler_properties> ──────────────────────────────────────────────────
#> • allow_multiple_parameters: `FALSE`
#> • requires_gradient: `FALSE`
#> • requires_allow_multiple_parameters: `FALSE`
#> • requires_deterministic: `FALSE`
#> ℹ Unset: 'restartable' and 'has_state'
```
