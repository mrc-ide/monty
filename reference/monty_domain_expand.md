# Expand (and check) domain against a packer

Check and expand a domain, where it is used alongside a
[monty_packer](https://mrc-ide.github.io/monty/reference/monty_packer.md)
object. This can be used to expand domains for logical parameters (e.g.
a vector `b`) into its specific names (e.g., `b[1]`, `b[2]`, etc)
without having to rely on the internals about how these names are
constructed.

## Usage

``` r
monty_domain_expand(domain, packer)
```

## Arguments

- domain:

  A two-column matrix as defined in
  [monty_model](https://mrc-ide.github.io/monty/reference/monty_model.md),
  with row names corresponding to either logical names (e.g., `b`) or
  specific names `b[1]` that are present in your packer. `NULL` is
  allowed where all parameters are defined over the entire real line.

- packer:

  A
  [monty_packer](https://mrc-ide.github.io/monty/reference/monty_packer.md)
  object

## Value

A two dimensional matrix representing your domain, or `NULL` if `domain`
was given as `NULL`.

## Examples

``` r
packer <- monty_packer(c("a", "b"), list(x = 3, y = c(2, 2)))
monty_domain_expand(NULL, packer)
#> NULL
monty_domain_expand(rbind(x = c(0, 1)), packer)
#>      [,1] [,2]
#> x[1]    0    1
#> x[2]    0    1
#> x[3]    0    1
monty_domain_expand(rbind(x = c(0, 1), "x[2]" = c(0, Inf)), packer)
#>      [,1] [,2]
#> x[1]    0    1
#> x[2]    0  Inf
#> x[3]    0    1
monty_domain_expand(rbind(x = c(0, 1), "y" = c(0, Inf)), packer)
#>        [,1] [,2]
#> x[1]      0    1
#> x[2]      0    1
#> x[3]      0    1
#> y[1,1]    0  Inf
#> y[2,1]    0  Inf
#> y[1,2]    0  Inf
#> y[2,2]    0  Inf
```
