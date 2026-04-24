# Compute gradient of log density

Compute the gradient of log density (which is returned by
[monty_model_density](https://mrc-ide.github.io/monty/reference/monty_model_density.md))
with respect to parameters. Not all models support this, and an error
will be thrown if it is not possible.

## Usage

``` r
monty_model_gradient(model, parameters, named = FALSE)
```

## Arguments

- model:

  A
  [monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
  object

- parameters:

  A vector or matrix of parameters

- named:

  Logical, indicating if the output should be named using the parameter
  names.

## Value

A vector or matrix of gradients

## See also

[monty_model_density](https://mrc-ide.github.io/monty/reference/monty_model_density.md)
for log density, and
[monty_model_direct_sample](https://mrc-ide.github.io/monty/reference/monty_model_direct_sample.md)
to sample from a model

## Examples

``` r
m <- monty_example("banana")
# Global maximum at (0, 0), and gradient is zero there:
monty_model_density(m, c(0, 0))
#> [1] -1.837877
monty_model_gradient(m, c(0, 0))
#> [1] 0 0

# Nonzero gradient away from the origin:
monty_model_gradient(m, c(0.4, 0.2))
#> [1] -1.440  0.376
```
