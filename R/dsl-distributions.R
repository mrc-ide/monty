## We will expand this later to support differentiation; that might
## impact the density calculation depending on how we handle the
## symbolic differentiation vs the automatic differentiation.
##
## We also need the domain from the distributions, but the uniform
## makes this surprisingly hard because it is either domain [a, b]
## with constant parameters or it is (-Inf, Inf) otherwise!
distribution <- function(name, density, sample = NULL) {
  args <- names(formals(density))[-1]
  list(name = name,
       args = args,
       density = density,
       sample = sample)
}


distr_exponential_rate <- distribution(
  name = "Exponential",
  density = function(x, rate) dexp(x, rate, log = TRUE),
  sample = function(rng, rate) rng$exponential(1, rate))

distr_exponential_mean <- distribution(
  name = "Exponential",
  density = function(x, mean) dexp(x, 1 / mean, log = TRUE),
  sample = function(rng, mean) rng$exponential(1, 1 / mean))

distr_normal <- distribution(
  name = "Normal",
  density = function(x, mean, sd) dnorm(x, mean, sd, log = TRUE),
  sample = function(rng, mean, sd) rng$normal(1, mean, sd))

distr_uniform <- distribution(
  name = "Uniform",
  density = function(x, min, max) dunif(x, min, max, log = TRUE),
  sample = function(rng, min, max) rng$uniform(1, min, max))

dsl_distributions <- local({
  d <- list(
    distr_exponential_rate, # preferred form, listed first
    distr_exponential_mean,
    distr_normal,
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
  if (is.null(nms)) {
    given <- paste(rep(".", length(args)), collapse = ", ")
  } else {
    given <- paste(ifelse(nzchar(args), args, "."), collapse = ", ")
  }
  detail <- c("x" = "Failed to match given argumnents: {given}")

  expected <- vcapply(candidates, function(x) paste(names(x), collapse = ", "))
  if (length(candidates) == 1) {
    hint <- c("i" = "Call should match:")
  } else {
    hint <- c("i" = "Call should match one of:")
  }
  list(success = FALSE, error = c(detail, hint, set_names(expected, ">")))
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
  j <- match(nms[i], names(candidate))
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
