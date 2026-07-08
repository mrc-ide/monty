# Create `monty_model` from a function computing density

Create a
[monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
from a function that computes density. This allows use of any R function
as a simple monty model. If you need advanced model features, then this
interface may not suit you and you may prefer to use
[monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
directly.

## Usage

``` r
monty_model_function(
  density,
  packer = NULL,
  fixed = NULL,
  domain = NULL,
  allow_multiple_parameters = FALSE
)
```

## Arguments

- density:

  A function to compute log density. It can take any number of
  parameters

- packer:

  Optionally, a
  [monty_packer](https://mrc-ide.github.io/monty/reference/monty_packer.md)
  object to control how your function parameters are packed into a
  numeric vector. You can typically omit this if all the arguments to
  your functions are present in your numeric vector and if they are all
  scalars.

- fixed:

  Optionally, a named list of fixed values to substitute into the call
  to `density`. This cannot be used in conjunction with `packer` (you
  should use the `fixed` argument to `monty_packer` instead).

- domain:

  Optional domain, see
  [monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)'s
  arguments for details. You can use "logical" names for array
  parameters and these will be expanded as described in
  [`monty_domain_expand()`](https://mrc-ide.github.io/monty/reference/monty_domain_expand.md).

- allow_multiple_parameters:

  Logical, indicating if passing in vectors for all parameters will
  return a vector of densities. This is `FALSE` by default because we
  cannot determine this automatically. Be aware that R's recycling rules
  may mean that this will not always work as expected!

## Value

A
[monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md)
object that computes log density with the provided `density` function,
given a numeric vector argument representing all parameters.

## Details

This interface will expand in future versions of monty to support
gradients, stochastic models, parameter groups and simultaneous
calculation of density.

## Examples

``` r
banana <- function(a, b, sd) {
  dnorm(b, log = TRUE) + dnorm((a - b^2) / sd, log = TRUE)
}
m <- monty_model_function(banana, fixed = list(sd = 0.25))
m
#> 
#> ── <monty_model> ───────────────────────────────────────────────────────────────
#> ℹ Model has 2 parameters: 'a' and 'b'
#> ℹ See `?monty_model()` for more information

# Density from our new model. Note that this computes density
# using an unstructured parameter vector, which is mapped to 'a'
# and 'b':
monty_model_density(m, c(0, 0))
#> [1] -1.837877

# Same as the built-in banana example:
monty_model_density(monty_example("banana"), c(0, 0))
#> [1] -1.837877

# You can constrain parameters, for example:
fn <- function(a, b) {
  dbeta(a, 2, 5, log = TRUE) + dnorm(b, log = TRUE)
}

# Here 'a' must lie in [0, 1] so we pass this through as 'domain':
pr <- monty_model_function(fn, domain = rbind(a = c(0, 1)))

# Now, out-of-bounds values will be converted to -Inf:
monty_model_density(pr, c(0.5, 0.5)) # ok
#> [1] -1.108477
monty_model_density(pr, c(0.5, 5)) # -Inf
#> [1] -13.48348
```
