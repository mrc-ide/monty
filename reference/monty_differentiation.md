# Differentiate expressions

Differentiate expressions in the monty DSL. This function is exported
for advanced use, and really so that we can use it from odin. But it has
the potential to be generally useful, so while we'll tweak the interface
quite a lot over the next while it is fine to use if you can handle some
disruption.

## Usage

``` r
monty_differentiation()
```

## Value

A list of related objects:

- `differentiate`: A function that can differentiate an expression with
  respect to a variable (as a string).

- `maths`: Some mathematical utilities for constructing expressions.
  This will be documented later, but the most useful bits on here are
  the function elements `times`, `plus` and `plus_fold`.

We will expand this soon to advertise what functions we are able to
differentiate to allow programs to fail fast.

## Details

R already has support for differentiating expressions using
[D](https://rdrr.io/r/stats/deriv.html), which is useful for creating
derivatives of simple functions to pass into non-linear optimisation. We
need something a bit more flexible for differentiating models in the
monty DSL
([monty_dsl](https://mrc-ide.github.io/monty/reference/monty_dsl.md))
and also in the related odin DSL.

## Differences to [`D()`](https://rdrr.io/r/stats/deriv.html)

- We try a little harder to simplify expressions.

- The distribution functions in the monty DSL (e.g., Poisson) are (will
  be) handled specially, allowing substitution of log-densities and
  expectations.

- Once we support array expressions, we will be able to differentiate
  through these.

## Roadmap

We may need to make this slightly extensible in future, but for now the
set of functions that can be differentiated is closed.

## Warning

The way of accessing distribution support here is peculiar and the
return type unusual. This is intentional, and we expect a more
conventional interface in the future once this package settles down.

## Examples

``` r
d <- monty_differentiation()
d$differentiate(quote(sqrt(sin(x))), "x")
#> cos(x)/(2 * sqrt(sin(x)))
D(quote(sqrt(sin(x))), "x")
#> 0.5 * (cos(x) * sin(x)^-0.5)
```
