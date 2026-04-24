# Parallel Tempering Sampler

Create a "parallel tempering" sampler, which runs multiple chains at
once to try and improve mixing, or takes advantage of
vectorisation/parallelisation if your underlying model supports it. We
have tested the implementation with the random walk sampler
([monty_sampler_random_walk](https://mrc-ide.github.io/monty/reference/monty_sampler_random_walk.md))
but this may work with other samplers.

## Usage

``` r
monty_sampler_parallel_tempering(
  sampler,
  n_rungs = NULL,
  beta = NULL,
  base = NULL
)
```

## Arguments

- sampler:

  A sampler to use for the underlying chains. You might use something
  like `monty_sampler_random_walk(vcv)` for a random walk sampler
  embedded in this parallel tempering scheme.

- n_rungs:

  The number of **extra** chains to run, must be at least 1. We will run
  a total of `n_rungs + 1` chains, with one of these being your target
  distribution and one being a direct sample from your base model (often
  your prior).

- beta:

  A vector of beta values. If provided, then `n_rungs` should not be
  provided, and `beta` should be a vector of at least length 2, where
  the first value is 1, the last value is 0 and the values in between
  form a strictly decreasing sequence (i.e., no increases, no ties).

- base:

  An optional base model, which must be provided if your model cannot be
  automatically decomposed into `prior + posterior` using
  [monty_model_split](https://mrc-ide.github.io/monty/reference/monty_model_split.md),
  or if you are not using this within a Bayesian context and you want to
  use an alternative easy-to-sample-from reference distribution. We
  require that this model can be directly sampled from, that it accepts
  multiple parameters (as a matrix), that it is deterministic and we
  assume that it is cheap to compute (relative to the target).

## Value

A `monty_sampler` object, which can be used with
[monty_sample](https://mrc-ide.github.io/monty/reference/monty_sample.md)

## Details

We implement the sampler based on <https://doi.org/10.1111/rssb.12464>

## Efficiency of the sampler

A parallel tempering sampler runs a series of chains at the same time,
so is doing much more work than a simpler sampler. If you run with
`n_rungs = 10` you are doing 11x more work than the underlying base
sampler, so you want to make sure that this is paid back somewhere.
There are a few places where this efficiency may come from:

1.  **Your model is parallelisable**. If your underlying model can run
    very efficiently in parallel then it may not take much longer in
    "wall time" to run the extra copies of the calculations. In this
    case, you'll still be using much more CPU time but will be able to
    take advantage of extra cores to get more effective sampling if the
    parallel tempering sampler mixes better than the underlying sampler.

2.  **Your model is vectorised**. If your model is implemented in R and
    vectorises the density calculations then it will generally not take
    much longer to compute many densities at once than a single one.

3.  **Your density is multimodal**. If your density has distinct peaks,
    then most samplers will struggle to explore it well, and even with a
    non-parallelised, non-vectorised sampler the parallel tempering
    sampler will explore the space more efficiently. In the limit, a
    normal sampler may only explore a single peak in a model with many
    such peaks and will never mix properly.

## Tuning the beta values

The argument `beta` controls the spacing of temperature among chains.
Ideally, this is set so that adjacent chains all have the same
probability of swapping, which generally means that the `beta` values
themselves will *not* be equidistant. Creating the appropriate vector of
betas (or "annealing schedule") requires running a pilot run, then
computing new `beta` values, then running another (new) chain with the
new beta values, repeating this iteration a few times until the beta
values stabilise. Rumour has it, this should only take a few iterations.

The updated values of `beta` are stored in the `details` of your samples
after running. So if you have a set of samples called `s`, then
`s$details$beta` will contain new beta values that you can use on the
next run.

It is important not to concatenate chains that are computed with
different `beta` values as they will not generally represent samples
from the target distribution (as an extreme case, consider one set of
beta values where we never accept swaps onto the target distribution and
another where we always do; we obviously cannot concatenate these).
Therefore, every time that you change beta values you should start a new
chain. You can do this with
[monty_sample_continue](https://mrc-ide.github.io/monty/reference/monty_sample_continue.md)
so long as you pass `append = FALSE`.
