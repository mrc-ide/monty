# Split a combined model

Split a model that has been combined by
[`monty_model_combine()`](https://mrc-ide.github.io/monty/reference/monty_model_combine.md)
into its constituent parts.

## Usage

``` r
monty_model_split(model, prior_first = FALSE)
```

## Arguments

- model:

  A combined model

- prior_first:

  Logical, indicating that we should require that the model component
  that could be the prior is listed first. If `TRUE` and one component
  model is not plausibly the prior, we will error. See Details for the
  heuristic used.

## Value

An unnamed list of length 2, being the component models. If one model
might be the prior it will be listed first.

## Details

We assume that a split model can be broken into a "prior" and a
"likelihood" if exactly one model:

- can be directly sampled from

- is not stochastic

- consumes all parameters

Typically, it will be the first criterion that will separate a model
into prior and likelihood (if you could sample from your likelihood,
then you would not use a sampler, which is where we are typically going
to perform this action).

If `prior_first` is `FALSE` we just return the parts.
