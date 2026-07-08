# Compute log density

Compute log density for a model. This is a wrapper around the `$density`
property within a
[monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
object.

## Usage

``` r
monty_model_density(model, parameters)
```

## Arguments

- model:

  A
  [monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
  object

- parameters:

  A vector or matrix of parameters

## Value

A log-density value, or vector of log-density values

## See also

[monty_model_gradient](https://mrc-ide.github.io/monty/reference/monty_model_gradient.md)
for computing gradients and
[monty_model_direct_sample](https://mrc-ide.github.io/monty/reference/monty_model_direct_sample.md)
for sampling from a model.

## Examples

``` r
m <- monty_model_function(function(a, b) dnorm(0, a, b, log = TRUE))
monty_model_density(m, c(0, 1))
#> [1] -0.9189385
monty_model_density(m, c(0, 10))
#> [1] -3.221524
```
