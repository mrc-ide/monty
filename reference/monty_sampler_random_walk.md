# Random Walk Sampler

Create a simple random walk sampler, which uses a symmetric proposal to
move around parameter space. This sampler supports sampling from models
where the likelihood is only computable randomly (e.g., for pmcmc).

## Usage

``` r
monty_sampler_random_walk(
  vcv,
  boundaries = "reflect",
  rerun_every = Inf,
  rerun_random = TRUE
)
```

## Arguments

- vcv:

  A variance covariance matrix for the proposal.

- boundaries:

  Control the behaviour of proposals that are outside the model domain.
  The supported options are:

  - "reflect" (the default): we reflect proposed parameters that lie
    outside the domain back into the domain (as many times as needed)

  - "reject": we do not evaluate the density function, and return `-Inf`
    for its density instead.

  - "ignore": evaluate the point anyway, even if it lies outside the
    domain.

  The initial point selected will lie within the domain, as this is
  enforced by
  [monty_sample](https://mrc-ide.github.io/monty/reference/monty_sample.md).

- rerun_every:

  Optional integer giving the frequency at which we should rerun the
  model on the current "accepted" parameters to obtain a new density for
  stochastic models. The default for this (`Inf`) will never trigger a
  rerun, but if you set to 100, then every 100 steps we run the model on
  both the proposed *and* previously accepted parameters before doing
  the comparison. This may help "unstick" chains, at the cost of some
  bias in the results.

- rerun_random:

  Logical, controlling the behaviour of rerunning (when `rerun_every` is
  finite). With the default value of `TRUE`, we stochastically rerun at
  each step with probability of `1 / rerun_every`. If `FALSE` we rerun
  the model at fixed intervals of iterations (given by `rerun_every`).
  The two methods give the same expected number of MCMC steps between
  reruns but a different pattern.

## Value

A `monty_sampler` object, which can be used with
[monty_sample](https://mrc-ide.github.io/monty/reference/monty_sample.md)
