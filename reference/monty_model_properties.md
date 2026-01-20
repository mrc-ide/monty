# Describe model properties

Describe properties of a model. Use of this function is optional, but
you can pass the return value of this as the `properties` argument of
monty_model to enforce that your model does actually have these
properties.

## Usage

``` r
monty_model_properties(
  has_gradient = NULL,
  has_direct_sample = NULL,
  is_stochastic = NULL,
  has_parameter_groups = NULL,
  has_observer = NULL,
  allow_multiple_parameters = NULL
)
```

## Arguments

- has_gradient:

  Logical, indicating if the model has a `gradient` method. Use `NULL`
  (the default) to detect this from the model.

- has_direct_sample:

  Logical, indicating if the model has a `direct_sample` method. Use
  `NULL` (the default) to detect this from the model.

- is_stochastic:

  Logical, indicating if the model is stochastic. Stochastic models must
  supply `set_rng_state` and `get_rng_state` methods.

- has_parameter_groups:

  Logical, indicating that the model can be decomposed into parameter
  groups which are independent of each other. This is indicated by using
  the `parameter_groups` field within the `model` object passed to
  [monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md),
  and by the presence of a `by_group` argument to `density` and (later
  we may also support this in `gradient`). Use `NULL` (the default) to
  detect this from the model.

- has_observer:

  Logical, indicating if the model has an "observation" function, which
  we will describe more fully soon. An observer is a function `observe`
  which takes no arguments and returns arbitrary data about the
  previously evaluated density. Use `NULL` (the default) to detect this
  from the model.

- allow_multiple_parameters:

  Logical, indicating if the density calculation can support being
  passed a matrix of parameters (with each column corresponding to a
  different parameter set) and return a vector of densities. If `FALSE`,
  we will support some different approaches to sort this out for you if
  this feature is needed. This cannot be detected from the model, and
  the default is `FALSE` because it's not always straightforward to
  implement. However, where it is possible it may be much more efficient
  (via vectorisation or parallelisation) to do this yourself.

## Value

A list of class `monty_model_properties` which should not be modified.

## Examples

``` r
# Default properties:
monty_model_properties()
#> 
#> ── <monty_model_properties> ────────────────────────────────────────────────────
#> ℹ Unset: 'has_gradient', 'has_direct_sample', 'is_stochastic', 'has_parameter_groups', 'has_observer', and 'allow_multiple_parameters'

# Set some properties:
monty_model_properties(has_gradient = TRUE, is_stochastic = FALSE)
#> 
#> ── <monty_model_properties> ────────────────────────────────────────────────────
#> • has_gradient: `TRUE`
#> • is_stochastic: `FALSE`
#> ℹ Unset: 'has_direct_sample', 'has_parameter_groups', 'has_observer', and 'allow_multiple_parameters'
```
