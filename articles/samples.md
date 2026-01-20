# Working with samples

This vignette outlines some ways of working with samples generated from
running an MCMC with `monty_sample`. We’ll flesh this out as we develop
nicer ways of working with samples, and mostly exists so that we can
point to it from other documentation, including from other packages
(e.g., from [`odin2`](https://mrc-ide.github.io/odin2)).

``` r
library(monty)
```

## The structure of monty samples

All samplers and runners produce samples with the same basic structure.
This structure is documented (here) so you are free to use this
structure if you want to just dive in and manipulate things. Please
treat output as read-only; extract data all you want, but make a copy
and don’t change any value within the samples structure if you are going
to pass it back into a `monty` function, as we assume that they have not
been modified.

Below, `samples` is a poorly mixed result of samples with 2 parameters,
2000 samples, and 4 chains. It was the result of running
[`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)
on the banana model from
[`vignette("samplers")`](https://mrc-ide.github.io/monty/articles/samplers.md),
and has class `monty_samples`.

``` r
samples
#> 
#> ── <monty_samples: 2 parameters x 2000 samples x 4 chains> ─────────────────────
#> ℹ Parameters: 'alpha' and 'beta'
#> ℹ Conversion to other types is possible:
#> → ! posterior::as_draws_array() [package installed, but not loaded]
#> → ! posterior::as_draws_df() [package installed, but not loaded]
#> → ! coda::as.mcmc.list() [package installed, but not loaded]
#> ℹ See `?monty_sample()` and `vignette("samples")` for more information
```

Each `monty_samples` object contains an element `pars` which contains
sampled parameters

``` r
dim(samples$pars)
#> [1]    2 2000    4
```

### Conversion to `posterior`’s `draw` objects

We implement methods for
[`posterior::as_draws_array`](https://mc-stan.org/posterior/reference/draws_array.html)
and
[`posterior::as_draws_df`](https://mc-stan.org/posterior/reference/draws_df.html),
which you can use to convert to formats that you might be familiar with
from use with other statistical packages. This only preserves the
parameters (`$pars`) from above.

``` r
samples_df <- posterior::as_draws_df(samples)
```

With this, you can access summary methods already implemented elsewhere:

``` r
posterior::summarise_draws(samples_df)
#> # A tibble: 2 × 10
#>   variable  mean median    sd   mad     q5   q95  rhat ess_bulk ess_tail
#>   <chr>    <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 alpha    2.27   0.645  3.06  1.39 -0.612  7.73  1.85     5.79     43.5
#> 2 beta     0.608  0.364  1.39  1.02 -2.05   2.78  2.10     5.32     11.7
```

With these objects you should be able to use any of the plotting
functions in [`bayesplot`](https://mc-stan.org/bayesplot), for example,
[MCMC visual
diagnostics](https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html),
without much trouble.

We also support conversion to `draws_array`; let us know if you need the
other conversion functions (`as_draws_list`, `as_draws_rvars`,
`as_draws_matrix`).

### Conversion to `coda`’s `mcmc.list` objects

The `coda` package has utilities for working with MCMC, and many other
packages are compatible with its `mcmc.list` object type (e.g., the
[`ggmcmc`](https://cran.r-project.org/package=ggmcmc) package). We
provide a method for `coda`’s `as.mcmc.list`, if that package is
available:

``` r
samples_coda <- coda::as.mcmc.list(samples)
coda::effectiveSize(samples_coda)
#>    alpha     beta 
#> 39.92519 68.47306
```
