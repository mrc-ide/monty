# Information about supported distributions

Report information about supported distributions in the DSL. This is
primarily intended for use in packages which use
[monty_dsl_parse_distribution](https://mrc-ide.github.io/monty/reference/monty_dsl_parse_distribution.md),
as this function reports information about which distributions and
arguments would succeed there.

## Usage

``` r
monty_dsl_distributions()
```

## Value

A data.frame with columns

- `name` the name of the distribution; each name begins with a capital
  letter, and there are duplicate names where different
  parameterisations are supported.

- `args` the arguments of all parameters, *except* the random variable
  itself which is given as the first argument to density functions.

We may expand the output here in the future to include information on if
distributions have support in C++, but we might end up supporting
everything this way soon.

## Examples

``` r
monty_dsl_distributions()
#>                  name         args
#> 1                Beta         a, b
#> 2        BetaBinomial size, pr....
#> 3        BetaBinomial   size, a, b
#> 4            Binomial   size, prob
#> 5              Cauchy location....
#> 6         Exponential         rate
#> 7         Exponential         mean
#> 8               Gamma  shape, rate
#> 9               Gamma shape, scale
#> 10     Hypergeometric      m, n, k
#> 11          LogNormal meanlog,....
#> 12   NegativeBinomial   size, prob
#> 13   NegativeBinomial     size, mu
#> 14             Normal     mean, sd
#> 15            Poisson       lambda
#> 16    TruncatedNormal mean, sd....
#> 17            Uniform     min, max
#> 18            Weibull shape, scale
#> 19 ZINegativeBinomial pi0, siz....
#> 20 ZINegativeBinomial pi0, siz....
#> 21          ZIPoisson  pi0, lambda
```
