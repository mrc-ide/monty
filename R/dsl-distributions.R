## We will expand this later to support differentiation; that might
## impact the density calculation depending on how we handle the
## symbolic differentiation vs the automatic differentiation.
##
## We also need the domain from the distributions, but the uniform
## makes this surprisingly hard because it is either domain [a, b]
## with constant parameters or it is (-Inf, Inf) otherwise!
distribution <- function(name, density, domain, cpp,
                         sample = NULL, variant = NULL) {
  args <- names(formals(density))[-1]
  list(name = name,
       variant = variant,
       args = args,
       density = density,
       domain = domain,
       sample = sample,
       cpp = cpp)
}


distr_binomial <- distribution(
  name = "Binomial",
  density = function(x, size, prob) dbinom(x, size, prob, log = TRUE),
  domain = c(0, Inf),
  sample = function(rng, size, prob) rng$binomial(1, size, prob),
  cpp = list(density = NULL, sample = "binomial"))

distr_exponential_rate <- distribution(
  name = "Exponential",
  variant = "rate",
  density = function(x, rate) dexp(x, rate, log = TRUE),
  domain = c(0, Inf),
  sample = function(rng, rate) rng$exponential(1, rate),
  cpp = list(density = NULL, sample = "exponential"))

distr_exponential_mean <- distribution(
  name = "Exponential",
  variant = "mean",
  density = function(x, mean) dexp(x, 1 / mean, log = TRUE),
  domain = c(0, Inf),
  sample = function(rng, mean) rng$exponential(1, 1 / mean),
  cpp = list(density = NULL, sample = NULL))

distr_normal <- distribution(
  name = "Normal",
  density = function(x, mean, sd) dnorm(x, mean, sd, log = TRUE),
  domain = c(-Inf, Inf),
  sample = function(rng, mean, sd) rng$normal(1, mean, sd),
  cpp = list(density = "normal", sample = "normal"))

distr_poisson <- distribution(
  name = "Poisson",
  density = function(x, lambda) dpois(x, lambda, log = TRUE),
  domain = c(0, Inf),
  sample = function(rng, lambda) rng$poisson(1, lambda),
  cpp = list(density = "poisson", sample = "poisson"))

distr_uniform <- distribution(
  name = "Uniform",
  density = function(x, min, max) dunif(x, min, max, log = TRUE),
  sample = function(rng, min, max) rng$uniform(1, min, max),
  domain = function(min, max) {
    c(if (is.na(min)) -Inf else min, if (is.na(max)) Inf else max)
  },
  cpp = list(density = NULL, sample = "uniform"))

dsl_distributions <- local({
  d <- list(
    distr_binomial,
    distr_exponential_rate, # preferred form, listed first
    distr_exponential_mean,
    distr_normal,
    distr_poisson,
    distr_uniform)
  split(d, vapply(d, "[[", "", "name"))
})


## Utilities to match calls against lists; this would go in util.R but
## it's really very geared around what we need for validating and
## disambiguating calls to distribution function
match_call <- function(args, candidates) {
  for (i in seq_along(candidates)) {
    res <- match_call_candidate(args, candidates[[i]])
    if (!is.null(res)) {
      return(list(success = TRUE, index = i, args = res))
    }
  }

  ## Everything below here is error handling; we've failed to match
  ## the call.
  nms <- names(args)
  unnamed <- sprintf("<%d>", seq_along(args))
  if (is.null(nms)) {
    given <- paste(unnamed, collapse = ", ")
  } else {
    given <- paste(ifelse(nzchar(args), args, unnamed), collapse = ", ")
  }
  detail <- c("x" = sprintf("Failed to match given arguments: %s", given))

  expected <- vcapply(candidates, paste, collapse = ", ")
  if (length(candidates) == 1) {
    hint <- c("i" = "Call should match:")
  } else {
    hint <- c("i" = "Call should match one of:")
  }
  list(success = FALSE, error = c(detail, hint, set_names(expected, "*")))
}


match_call_candidate <- function(args, candidate) {
  if (length(args) != length(candidate)) {
    return(NULL)
  }
  nms <- names(args)
  if (is.null(nms)) {
    return(seq_along(args))
  }
  i <- nzchar(nms)
  if (anyDuplicated(nms[i])) {
    return(NULL)
  }
  j <- match(nms[i], candidate)
  if (anyNA(j)) {
    return(NULL)
  }
  ## This is the same logic as used in R's argument matching, which slots in
  ## named arguments then assigns the remaining as unnamed arguments.
  n <- length(args)
  ret <- integer(n)
  ret[i] <- j
  ret[seq_len(n)[!i]] <- seq_len(n)[-j]
  ret
}
