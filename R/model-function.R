##' Create a [monty_model] from a function that computes density.
##' This allows use of any R function as a simple monty model.  If you
##' need advanced model features, then this interface may not suit you
##' and you may prefer to use [monty_model] directly.
##'
##' This interface will expand in future versions of monty to support
##' gradients, stochastic models, parameter groups and simultaneous
##' calculation of density.
##'
##' @title Create `monty_model` from a function computing density
##'
##' @param density A function to compute log density.  It can take any
##'   number of parameters
##'
##' @param packer Optionally, a [monty_packer] object to control how
##'   your function parameters are packed into a numeric vector.  You
##'   can typically omit this if all the arguments to your functions
##'   are present in your numeric vector and if they are all scalars.
##'
##' @param fixed Optionally, a named list of fixed values to
##'   substitute into the call to `density`.  This cannot be used in
##'   conjunction with `packer` (you should use the `fixed` argument
##'   to `monty_packer` instead).
##'
##' @param domain Optional domain, see [monty_model]'s arguments for
##'   details.
##'
##' @param allow_multiple_parameters Logical, indicating if passing in
##'   vectors for all parameters will return a vector of densities.
##'   This is `FALSE` by default because we cannot determine this
##'   automatically.  Be aware that R's recycling rules may mean that
##'   this will not always work as expected!
##'
##' @return A [monty_model] object that computes log density with the
##'   provided `density` function, given a numeric vector argument
##'   representing all parameters.
##'
##' @export
##' @examples
##' banana <- function(a, b, sd) {
##'   dnorm(b, log = TRUE) + dnorm((a - b^2) / sd, log = TRUE)
##' }
##' m <- monty_model_function(banana, fixed = list(sd = 0.25))
##' m
##'
##' # Density from our new model. Note that this computes density
##' # using an unstructured parameter vector, which is mapped to 'a'
##' # and 'b':
##' monty_model_density(m, c(0, 0))
##'
##' # Same as the built-in banana example:
##' monty_model_density(monty_example("banana"), c(0, 0))
monty_model_function <- function(density, packer = NULL, fixed = NULL,
                                 domain = NULL,
                                 allow_multiple_parameters = FALSE,
                                 is_stochastic = FALSE) {
  if (!is.function(density)) {
    cli::cli_abort("Expected 'density' to be a function", arg = "density")
  }

  if (!is.null(fixed)) {
    assert_named(fixed, unique = TRUE)
    assert_list(fixed)
  }

  if (is.null(packer)) {
    packer <- monty_packer(
      setdiff(names(formals(density)), names(fixed)))
  } else {
    assert_is(packer, "monty_packer")
    if (!is.null(fixed)) {
      cli::cli_abort("Can't provide both 'packer' and 'fixed'", arg = "fixed")
    }

    if (allow_multiple_parameters) {
      inputs <- packer$inputs()
      if (!is.null(inputs$process)) {
        cli::cli_abort(paste(
          "Can't use 'allow_multiple_parameters' with a packer that uses",
          "'process'"))
      }
      if (!is.null(inputs$fixed)) {
        fixed <- inputs$fixed
        packer <- monty_packer(array = inputs$array)
      }
    }
  }

  properties <- monty_model_properties(
    allow_multiple_parameters = allow_multiple_parameters,
    is_stochastic = is_stochastic)

  parameters <- packer$names()

  use_domain <- !is.null(domain)
  if (use_domain) {
    domain <- monty::monty_domain_expand(domain, packer)
    domain <- validate_domain(domain, parameters, call = environment())
    if (allow_multiple_parameters) {
      ## This involves some pretty tedious bookkeeping, and is going
      ## to interact with the interface for running an indexed subset
      ## of parameters that we need to sort dust out properly.
      cli::cli_abort(
        "'allow_multiple_parameters' and 'domain' cannot yet be used together")
    }
 }

  monty_model(
    list(parameters = parameters,
         density = function(x) {
           if (use_domain && !all(x >= domain[, 1] & x <= domain[, 2])) {
             return(-Inf)
           }
           rlang::inject(density(!!!packer$unpack(x), !!!fixed))
         },
         domain = domain),
    properties)
}
