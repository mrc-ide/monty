# monty

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R-CMD-check](https://github.com/mrc-ide/monty/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/mrc-ide/monty/actions/workflows/R-CMD-check.yaml)
[![codecov.io](https://codecov.io/github/mrc-ide/monty/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/monty?branch=main)
<!-- badges: end -->

`monty` is a toolbox for Monte Carlo methods.  It is designed to help run MCMC with models that do not fit closely within the paradigms of packages such as [stan](https://mc-stan.org/) - for example some likelihood you have written by hand.

See the [package vignette](https://mrc-ide.github.io/monty/articles/monty.html) for a basic introduction.

The package is in an early, though rapid, stage of development and we cannot guarantee that interfaces will not change (in fact, we can guarantee that the **will** really).  However the core concepts have now solidified.

The core of the package is build around some basic ideas:

* you have some [statistical model](https://mrc-ide.github.io/monty/reference/monty_model.html) from which you would like to draw samples using MCMC ([`monty::monty_sample`](https://mrc-ide.github.io/monty/reference/monty_sample.html))
* you can choose between different [samplers](https://mrc-ide.github.io/monty/articles/samplers.html) depending on the [properties of your model](https://mrc-ide.github.io/monty/reference/monty_model_properties.html)
* you can specify priors for your model with a [high-level DSL](https://mrc-ide.github.io/monty/articles/dsl.html)
* you can power stochastic models with a fast, parallelisable, [random number generator](https://mrc-ide.github.io/monty/reference/monty_rng.html), and use this from both R and C++

## Related tools

`monty` is a complete rewrite of some of the ideas in [`mcstate`](https://mrc-ide.github.io/mcstate/), though other bits of `mcstate` have moved into [`dust2`](https://mrc-ide.github.io/dust2/).

The [`dust2`](https://mrc-ide.github.io/dust2/) package provides machinery to run particle filters for sequential Monte Carlo methods using the random number support in `monty` and can create statistical models that can be used with `monty`'s samplers

The [`odin2`](https://mrc-ide.github.io/odin2/) package can generate dynamical models that use `monty`'s random number generators and can be used as part of `dust2` particle filter.  The `odin2` DSL and `monty` DSL are closely related.

## Roadmap

The basic functionality of this package is in place, but we plan to do work on:

* expanding runners to include parallel chains (a basic parallel runner exists, but we will port over the `callr`-based runner from `mcstate`)
* improving debugging tools if models fail mid-chain
* implementing parallel tempering, allowing you to compose with any other supported sampler
* improving support for nested models
* improving and expanding the DSL

## Installation

Please install from our [r-universe](https://mrc-ide.r-universe.dev/):

```r
install.packages(
  "monty",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
```

If you prefer, you can install from GitHub with `remotes`:

```r
remotes::install_github("mrc-ide/monty", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
