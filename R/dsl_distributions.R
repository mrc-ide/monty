## Collect all the bits together that define a distribution.  We'll
## probably expand this over time (for example, we could fairly easily
## look up things like the mean here if we want to).
distribution <- function(name, domain, density, sample, gradient) {
  list(name = name,
       domain = domain,
       density = density,
       sample = sample,
       gradient = gradient)
}


## Individual distributions below.  Just a couple of examples to get
## us going.
distribution_normal <- distribution(
  name = "Normal",
  domain = c(-Inf, Inf),
  density = function(x, mean, sd) dnorm(x, mean, sd, log = TRUE),
  sample = function(rng, mean, sd) rng$normal(1, mean, sd),
  gradient = function(x, mean, sd) (x - mean) / sd^2)


distribution_uniform <- distribution(
  name = "Uniform",
  domain = function(min, max) c(min, max),
  density = function(x, min, max) dunif(x, min, max, log = TRUE),
  sample = function(rng, min, max) rng$uniform(1, min, max),
  gradient = function(x, min, max) 0)


## This one is tricky because there are two different parametrisations
## of the distribution, and we want to capture that.  Other examples
## of this include the negative binomial distribution.  The user will
## have to name the second argument here at the point of use (so
## Gamma(1, rate = 1)) to select the right distribution.
distribution_gamma <- distribution(
  name = "Gamma",
  density = list(
    function(x, shape, rate) dgamma(x, shape, rate, log = TRUE),
    function(x, shape, scale) dgamma(x, shape, scale = scale, log = TRUE)),
  sample = list(
    function(rng, shape, rate) rng$gamma(1, shape, 1 / rate),
    function(rng, shape, scale) rng$gamma(1, shape, scale)),
  gradient = list(
    function(x, shape, rate) (shape - 1) / x - rate,
    function(x, shape, scale) (shape - 1) / x - 1 / scale),
  domain = c(0, Inf))


distributions <- list(
  distribution_gamma,
  distribution_normal,
  distribution_uniform)
names(distributions) <- vapply(distributions, "[[", "", "name")


distribution_bind <- function(distribution, expr) {
  ## TODO: we need some source information here too, for nice errors.
  if (is.list(distribution$density)) {
    ## Select correct version
  } else {
    ## check number of args?
    args <- as.list(expr[-1])
    template <- formals(distribution$density)[-1]
    if (length(args) != length(template)) {
      stop("unexpected number of arguments")
    }
    if (!is.null(names(args))) {
      ## anyDuplicated
      ## match template
    }
    ## Then look up values
    if (any(vlapply(args, is.language))) {
      stop("can't interpolate values yet")
    }
    env <- new.env(parent = topenv())
    env$density <- distribution$density
    density <- as_function(
      alist(x = ),
      as.call(c(list(quote(density), quote(x)), args)),
      env)
    direct_sample <- as_function(
      alist(rng = ),
      as.call(c(list(quote(sample), quote(x)), args))
      env)
    gradient <- as_function(
      alist(x = ),
      as.call(c(list(quote(gradient), quote(x)), args))
      env)
  }
  browser()
}


## partial application
partial <- function(f, args) {

}
