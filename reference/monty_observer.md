# Create observer

Create an observer to extract additional details from your model during
the sampling process.

## Usage

``` r
monty_observer(observe, finalise = NULL, combine = NULL, append = NULL)
```

## Arguments

- observe:

  A function that will run with arguments `model` (the model that you
  passed in to
  [monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md))
  and `rng` (an rng object). This function should return a list. It is
  best if the list returned is named, with no duplicated names, and with
  return values that have the same exact dimensions for every iteration.
  If you do this, then you will not have to provide any of the following
  arguments, which are going to be hard to describe and worse to
  implement.

- finalise:

  A function that runs after a single chain has run, and you use to
  simplify across all samples drawn from that chain. Takes a single
  argument which is the list with one set of observations per sample.

- combine:

  A function that runs after all chains have run, and you use to
  simplify across chains. Takes a single argument, which is the list
  with one set of observations per chain.

- append:

  A function that runs after a continuation of chain has run (via
  [monty_sample_continue](https://mrc-ide.github.io/monty/reference/monty_sample_continue.md).
  Takes two arguments representing the fully simplified observations
  from the first and second chains.

## Value

An object with class `monty_observer` which can be passed in to
`monty_sample`.

## Details

Sometimes you want to extract additional information from your model as
your chain runs. The case we see this most is when running MCMC with a
particle filter (pmcmc); in this case while the likelihood calculation
is running we are computing lots of interesting quantities such as the
final state of the system (required for onward simulation) and filtered
trajectories through time. Because these are stochastic we can't even
just rerun the model with our sampled parameter sets, because the final
states that are recovered depend also on the random number generators
(practically we would not want to, as it is quite expensive to compute
these quantities).

The observer mechanism allows you to carry out arbitrary additional
calculations with your model at the end of the step.
