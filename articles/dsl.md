# Probabilistic DSL

`monty` includes a simple probabilistic domain-specific language (DSL)
that is inspired by `stan` and [Statistical
Rethinking](https://xcelab.net/rm/). It is designed to make some tasks a
bit easier, particularly when defining priors for your model. We expect
that this DSL is not sufficiently advanced to represent most interesting
models but it may get more clever and flexible in the future. In
particular we do not expect the DSL to be useful in writing likelihood
functions for comparison to data; we expect that if your model is simple
enough for this you would be better off using `stan` or some similarly
flexible system.

``` r
library(monty)
```

## Some simple examples

In chapter 4 of Statistical Rethinking, we build a regression model of
height with parameters $\alpha$, $\beta$ and $\sigma$. We can define the
model for the prior probability of this model in monty by running

``` r
prior <- monty_dsl({
  alpha ~ Normal(178, 20)
  beta ~ Normal(0, 10)
  sigma ~ Uniform(0, 50)
})
```

This will define a new
[`monty_model()`](https://mrc-ide.github.io/monty/reference/monty_model.md)
object that represents the prior, but with all the bits that we might
need depending on how we want to use it:

We have model parameters

``` r
prior$parameters
#> [1] "alpha" "beta"  "sigma"
```

These are defined in the order that they appear in your definition (so
`alpha` is first and `sigma` is last)

We can compute the domain for your model:

``` r
prior$domain
#>       [,1] [,2]
#> alpha -Inf  Inf
#> beta  -Inf  Inf
#> sigma    0   50
```

We can draw samples from the model if we provide a `monty_rng` object

``` r
rng <- monty_rng_create()
theta <- monty_model_direct_sample(prior, rng)
theta
#> [1] 167.700794  -5.085348   8.899977
```

We can compute the (log) density at a point in parameter space

``` r
monty_model_density(prior, theta)
#> [1] -11.31011
```

The computed properties for the model are:

``` r
prior$properties
#> 
#> ── <monty_model_properties> ────────────────────────────────────────────────────
#> • has_gradient: `TRUE`
#> • has_direct_sample: `TRUE`
#> • is_stochastic: `FALSE`
#> • has_parameter_groups: `FALSE`
#> • has_observer: `FALSE`
#> • allow_multiple_parameters: `TRUE`
```

## Calculations in the DSL

Sometimes it will be useful to perform calculations in the code; you can
do this with assignments. Most trivially, giving names to numbers may
help make code more understandable:

``` r
m <- monty_dsl({
  mu <- 10
  sd <- 2
  a ~ Normal(mu, sd)
})
```

You can also use this to do things like:

``` r
m <- monty_dsl({
  a ~ Normal(0, 1)
  b ~ Normal(0, 1)
  mu <- (a + b) / 2
  c ~ Normal(mu, 1)
})
```

Where `c` is drawn from a normal distribution with a mean that is the
average of `a` and `b`.

## Pass in fixed data

You can also pass in a list of data with values that should be available
in the DSL code. For example, our first example:

``` r
prior <- monty_dsl({
  alpha ~ Normal(178, 20)
  beta ~ Normal(0, 10)
  sigma ~ Uniform(0, 50)
})
```

Might be written as

``` r
fixed <- list(alpha_mean = 170, alpha_sd = 20,
              beta_mean = 0, beta_sd = 10,
              sigma_max = 50)
prior <- monty_dsl({
  alpha ~ Normal(alpha_mean, alpha_sd)
  beta ~ Normal(beta_mean, beta_sd)
  sigma ~ Uniform(0, sigma_max)
}, fixed = fixed)
```

Values you pass in this way are **fixed** (hence the name!) and cannot
be modified after the model object is created.
