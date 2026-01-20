# Explain monty error

Explain error codes produced by monty. This is a work in progress, and
we would like feedback on what is useful as we improve it. The idea is
that if you see an error you can link through to get more information on
what it means and how to resolve it. The current implementation of this
will send you to the rendered vignettes, but in the future we will
arrange for offline rendering too.

## Usage

``` r
monty_dsl_error_explain(code, how = "pretty")
```

## Arguments

- code:

  The error code, as a string, in the form `Exxx` (a capital "E"
  followed by three numbers)

- how:

  How to explain the error. Options are `pretty` (render pretty text in
  the console), `plain` (display plain text in the console) and `link`
  (browse to the online help).

## Value

Nothing, this is called for its side effect only

## Examples

``` r
monty_dsl_error_explain("E201")
#> 
#> ── E201 ────────────────────────────────────────────────────────────────────────
#> Duplicated relationships (with `~`).
#> 
#> Example:
#> 
#>     a ~ Normal(0, 1)
#>     b ~ Uniform(0, 1)
#>     a ~ Exponential(1) # <= error here
#> 
#> Relationships must be unique because each represents a parameter, and a
#> parameter can't be represented by two different distributions.
#> 
```
