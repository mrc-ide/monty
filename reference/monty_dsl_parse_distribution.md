# Parse distribution expression

Parse an expression as if it were a call to one of monty's distribution
functions (e.g., `Normal`, `Poisson`). This will fill in any defaults,
disambiguate where multiple parameterisations of the distribution are
available, and provide links through to the C++ API. This function is
designed for use from other packages that use monty, and is unlikely to
be useful to most users.

## Usage

``` r
monty_dsl_parse_distribution(expr, name = NULL)
```

## Arguments

- expr:

  An expression

- name:

  Name for the expression, used in constructing messages that you can
  use in errors.

## Value

A list; the contents of this are subject to change. However you can (to
a degree) rely on the following elements:

- `name`: The name of the distribution (e.g., `Normal`). This will be
  the same as the name of the function called in `expr`

- `variant`: The name of the distribution variant, if more than one is
  supported.

- `args`: The arguments that you provided, in position-matched order

- `cpp`: The names of the C++ entrypoint to use. This is a list with
  elements `density` and `sample` for the log-density and sampling
  functions, and `NULL` where these do not yet exist.

Currently we also include:

- `density`: A function to compute the log-density. This will likely
  change once we support creation of differentiable models because we
  will want to do something with the arguments provided!

- `sample`: A function to sample from the distribution, given (as a
  first argument) a `monty_rng` object (see
  [monty_rng_create](https://mrc-ide.github.io/monty/reference/monty_rng_create.md))

## Examples

``` r
# A successful match
monty_dsl_parse_distribution(quote(Normal(0, 1)))
#> $success
#> [1] TRUE
#> 
#> $value
#> $value$name
#> [1] "Normal"
#> 
#> $value$variant
#> NULL
#> 
#> $value$args
#> $value$args[[1]]
#> [1] 0
#> 
#> $value$args[[2]]
#> [1] 1
#> 
#> 
#> $value$density
#> function (x, mean, sd) 
#> dnorm(x, mean, sd, log = TRUE)
#> <bytecode: 0x55fcad5f37a8>
#> <environment: namespace:monty>
#> 
#> $value$domain
#> [1] -Inf  Inf
#> 
#> $value$sample
#> [1] "monty_random_normal"
#> 
#> $value$expr
#> $value$expr$density
#> -(x - mean)^2/(2 * sd^2) - log(2 * pi)/2 - log(sd)
#> 
#> $value$expr$mean
#> mean
#> 
#> 
#> $value$cpp
#> $value$cpp$density
#> [1] "normal"
#> 
#> $value$cpp$sample
#> [1] "normal"
#> 
#> 
#> 

# An unsuccessful match
monty_dsl_parse_distribution(quote(Normal()))
#> $success
#> [1] FALSE
#> 
#> $error
#>                                                                       x 
#>        "Invalid call to 'Normal()'" "Failed to match given arguments: " 
#>                                   i                                   * 
#>                "Call should match:"                          "mean, sd" 
#> 
```
