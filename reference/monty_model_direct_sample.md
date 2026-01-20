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
#> [1] 0.2045067 0.2863123
monty_model_direct_sample(m, r, named = TRUE)
#>     alpha      beta 
#> 1.7613960 0.1409638 

r <- monty_rng_create(n_streams = 3)
monty_model_direct_sample(m, r)
#>           [,1]        [,2]       [,3]
#> alpha 4.119168 -0.01824816 -0.7010775
#> beta  2.157869  0.95892593  0.3348243
monty_model_direct_sample(m, r, named = TRUE)
#>           [,1]      [,2]       [,3]
#> alpha 2.676928 0.1371878  0.4383688
#> beta  1.329980 0.3987461 -0.2044148
```
