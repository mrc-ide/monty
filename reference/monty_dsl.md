# Domain Specific Language for monty

Create a model using the monty DSL; this function will likely change
name in future, as will its interface.

## Usage

``` r
monty_dsl(x, type = NULL, gradient = NULL, fixed = NULL, domain = NULL)
```

## Arguments

- x:

  The model as an expression. This may be given as an expression, as a
  string, or as a path to a filename. Typically, we'll do a reasonable
  job of working out what you've provided but use the `type` argument to
  disambiguate or force a particular interpretation. The argument uses
  rlang's quosures to allow you to work with expressions directly; see
  examples for details.

- type:

  Force interpretation of the type of expression given as `x`. If given,
  valid options are `expression`, `text` or `file`.

- gradient:

  Control gradient derivation. If `NULL` (the default) we try and
  generate a gradient function for your model and warn if this is not
  possible. If `FALSE`, then we do not attempt to construct a gradient
  function, which prevents a warning being generated if this is not
  possible. If `TRUE`, then we will error if it is not possible to
  create a gradient function.

- fixed:

  An optional list of values that can be used within the DSL code.
  Anything you provide here is available for your calculations. In the
  interest of future compatibility, we check currently that all elements
  are scalars. In future this may become more flexible and allow passing
  environments, etc. Once provided, these values cannot be changed
  without rebuilding the model; they are fixed data. You might use these
  for hyperparameters that are fixed across a set of model runs, for
  example.

- domain:

  An optional domain. Normally this is not wanted, but you can use this
  to truncate the domain of one or more parameters. The domain is
  effectively applied *after* the calculations implied by the DSL. The
  density is not recalculated to reflect the change in the marginal
  density. Applying a domain will remove the ability to sample from the
  model, at least for now. See
  [monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
  for details on the format. The provided parameters must match the
  parameters of your model.

## Value

A
[monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
object derived from the expressions you provide.

## Examples

``` r
# Expressions that define models can be passed in with no quoting
monty_dsl(a ~ Normal(0, 1))
#> 
#> ── <monty_model> ───────────────────────────────────────────────────────────────
#> ℹ Model has 1 parameter: 'a'
#> ℹ This model:
#> • can compute gradients
#> • can be directly sampled from
#> • accepts multiple parameters
#> ℹ See `?monty_model()` for more information
monty_dsl({
  a ~ Normal(0, 1)
  b ~ Exponential(1)
})
#> 
#> ── <monty_model> ───────────────────────────────────────────────────────────────
#> ℹ Model has 2 parameters: 'a' and 'b'
#> ℹ This model:
#> • can compute gradients
#> • can be directly sampled from
#> • accepts multiple parameters
#> ℹ See `?monty_model()` for more information

# You can also pass strings
monty_dsl("a ~ Normal(0, 1)")
#> 
#> ── <monty_model> ───────────────────────────────────────────────────────────────
#> ℹ Model has 1 parameter: 'a'
#> ℹ This model:
#> • can compute gradients
#> • can be directly sampled from
#> • accepts multiple parameters
#> ℹ See `?monty_model()` for more information
```
