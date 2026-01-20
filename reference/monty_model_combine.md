# Combine two models

Combine two models by multiplication. We'll need a better name here. In
Bayesian inference we will want to create a model that represents the
multiplication of a likelihood and a prior (in log space) and it will be
convenient to think about these models separately. Multiplying
probabilities (or adding on a log scale) is common enough that there may
be other situations where we want to do this.

## Usage

``` r
monty_model_combine(a, b, properties = NULL, name_a = "a", name_b = "b")
```

## Arguments

- a:

  The first model

- b:

  The second model

- properties:

  A
  [monty_model_properties](https://mrc-ide.github.io/monty/reference/monty_model_properties.md)
  object, used to control (or enforce) properties of the combined model.

- name_a:

  Name of the first model (defaulting to 'a'); you can use this to make
  error messages nicer to read, but it has no other practical effect.

- name_b:

  Name of the first model (defaulting to 'b'); you can use this to make
  error messages nicer to read, but it has no other practical effect.

## Value

A
[monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
object

## Details

Here we describe the impact of combining a pair of models

- `density`: this is the sum of the log densities from each model

- `parameters`: the union of parameters from each model is taken

- `domain`: The most restrictive domain is taken for each parameter.
  Parameters that do not appear in one model are assumed to have
  infinite domain there.

- `gradient`: if *both* models define a gradient, this is the sum of the
  gradients. If either does not define a gradient, the resulting model
  will not have gradient support. Set `has_gradient = TRUE` within
  \`properties if you want to enforce that the combination is
  differentiable. If the models disagree in their parameters, parameters
  that are missing from a model are assumed (reasonably) to have a zero
  gradient.

- `direct_sample`: this one is hard to do the right thing for. If
  neither model can be directly sampled from that's fine, we don't
  directly sample. If only one model can be sampled from *and* if it can
  sample from the union of all parameters then we take that function
  (this is the case for a prior model when combined with a likelihood).
  Other cases will be errors, which can be avoided by setting
  `has_direct_gradient = FALSE` in `properties`.

- `is_stochastic`: a model is stochastic if *either* component is
  stochastic.

The properties of the model will be combined as above, reflecting the
properties of the joint model.

The `model` field will be an ordered, unnamed, list containing the two
elements corresponding to the first and second model (not the
`monty_model`, but the underlying model, perhaps?). This is the only
part that makes a distinction between the two models here; for all
components above they are equivalent.

## Examples

``` r
# A simple example; a model that contains something of interest,
# and a simple prior from monty_dsl
likelihood <- monty_example("banana")
prior <- monty_dsl({
  alpha ~ Normal(0, 1)
  beta ~ Normal(0, 10)
})
posterior <- likelihood + prior
posterior
#> 
#> ── <monty_model> ───────────────────────────────────────────────────────────────
#> ℹ Model has 2 parameters: 'alpha' and 'beta'
#> ℹ This model:
#> • can compute gradients
#> • accepts multiple parameters
#> ℹ See `?monty_model()` for more information

# The same thing, more explicitly:
monty_model_combine(likelihood, prior)
#> 
#> ── <monty_model> ───────────────────────────────────────────────────────────────
#> ℹ Model has 2 parameters: 'alpha' and 'beta'
#> ℹ This model:
#> • can compute gradients
#> • accepts multiple parameters
#> ℹ See `?monty_model()` for more information

# Control properties of the combined model:
monty_model_combine(likelihood, prior,
                    monty_model_properties(has_gradient = FALSE))
#> 
#> ── <monty_model> ───────────────────────────────────────────────────────────────
#> ℹ Model has 2 parameters: 'alpha' and 'beta'
#> ℹ This model:
#> • accepts multiple parameters
#> ℹ See `?monty_model()` for more information
```
