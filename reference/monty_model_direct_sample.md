# Directly sample from a model

Directly sample from a model. Not all models support this, and an error
will be thrown if it is not possible.

## Usage

``` r
monty_model_direct_sample(model, rng, named = FALSE)
```

## Arguments

- model:

  A
  [monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
  object

- rng:

  Random number state, created by
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md).
  Use of an RNG with more than one stream may or may not work as
  expected; this is something we need to tidy up (`mrc-5292`)

- named:

  Logical, indicating if the output should be named using the parameter
  names.

## Value

A vector or matrix of sampled parameters

## Examples

``` r
m <- monty_example("banana")

r <- monty_rng_create()
monty_model_direct_sample(m, r)
#> [1] 0.5265751 0.1226645
monty_model_direct_sample(m, r, named = TRUE)
#>       alpha        beta 
#>  0.03770516 -0.41060250 

r <- monty_rng_create(n_streams = 3)
monty_model_direct_sample(m, r)
#>             [,1]      [,2]      [,3]
#> alpha  0.8114299 0.7362596 0.6248517
#> beta  -1.0283483 0.8058749 0.7719408
monty_model_direct_sample(m, r, named = TRUE)
#>             [,1]      [,2]      [,3]
#> alpha -0.1071062 0.1761735  3.676308
#> beta  -0.2730366 0.6740360 -2.109755
```
