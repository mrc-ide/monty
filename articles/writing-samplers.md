# Writing samplers

This vignette describes how one can write samplers that can be used
within `monty`. A sampler describes how to propagate a set of parameters
one step along the Markov chain used in MCMC. Once you have written a
custom sampler, you can use it with your `monty_model` along with all
the monty runners via
[`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md).

In this vignette, we will design a sampler that samples by taking a
series of independent steps for each parameter; it forms (approximately)
a very poor form of a random walk sampler with a diagonal
variance-covariance matrix, but has the advantage of being simple to
implement and not useful enough to include in the package!

## When might you want to create a new sampler?

We don’t anticipate that many people will want to create a new sampler,
but you can do this if you have some new algorithm that you want to try.
This can be as simple as wanting to have a more flexible update scheme
that moves around your parameters in a different way than (say) the
random walk sampler, but it might be a sampler for a very different sort
of process. Anything where your process involves integer values will
want a new sampler, because all our existing ones assume that parameters
are all on the real line.

Our intention is that it will be easy enough to create a sampler, and
that these can function perfectly well outside of the monty package,
making the package easy to extend.

## A toy problem

``` r
library(monty)
```

Let’s draw samples from a 5-dimensional Gaussian with unit variance and
mean of **0**:

``` r
model <- monty_example("gaussian", diag(5))
```

## A toy sampler

Our toy sampler will sample a point on each axis, sequentially,
accepting or rejecting each. We’ll use a random normal proposal in each
dimension and our control parameter will be the standard deviation of
this proposal; a vector of length 5 in this case. This sampler is
stateless for now - that is, nothing is stored in state. Initialisation
is therefore trivial too as there is almost nothing to do. However, this
is where we can check that our `control` parameters make sense with the
`model` that we have been given:

``` r
toy_sampler_initialise <- function(state_chain, control, model, rng) {
  n_sd <- length(control$sd)
  n_pars <- length(model$parameters)
  if (n_sd != n_pars) {
    cli::cli_abort("'control$sd' has length {n_sd} but your model has {n_pars} parameter{?s}")
  }
  NULL
}
```

The `step` function is where the actual interesting work happens:

``` r
toy_sampler_step <- function(state_chain, state_sampler, control, model, rng) {
  pars <- state_chain$pars
  density <- state_chain$density
  sd <- control$sd
  n_pars <- length(sd) # same as length(model$parameters)

  for (i in seq_len(n_pars)) {
    pars_next <- pars
    pars_next[[i]] <- monty_random_normal(pars[[i]], sd[[i]], rng)
    density_next <- monty_model_density(model, pars_next)
    accept <- density_next > density ||
      density_next - density > log(monty_random_real(rng))
    if (accept) {
      pars <- pars_next
      density <- density_next
    }
  }

  # Put state back together and return
  state_chain$pars <- pars
  state_chain$density <- density
  state_chain
}
```

Then we can write a constructor, which takes `sd` as the argument:

``` r
toy_sampler <- function(sd) {
  control <- list(sd = sd)
  monty_sampler(
    "Toy Sampler",
    "toy_sampler",
    control,
    toy_sampler_initialise,
    toy_sampler_step)
}
```

That’s it. We can now use our new sampler on the toy problem:

``` r
sampler <- toy_sampler(rep(0.2, 5))
samples <- monty_sample(model, sampler, 100)
#> ⡀⠀ Sampling  ■                                |   1% ETA:  3s
#> ✔ Sampled 100 steps across 1 chain in 73ms
#> 
samples
#> 
#> ── <monty_samples: 5 parameters x 100 samples x 1 chain> ───────────────────────
#> ℹ Parameters: 'a', 'b', 'c', 'd', and 'e'
#> ℹ Conversion to other types is possible:
#> → ! posterior::as_draws_array() [package installed, but not loaded]
#> → ! posterior::as_draws_df() [package installed, but not loaded]
#> → ! coda::as.mcmc.list() [package installed, but not loaded]
#> ℹ See `?monty_sample()` and `vignette("samples")` for more information
```
