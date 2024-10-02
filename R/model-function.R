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
monty_model_function <- function(density, packer = NULL, fixed = NULL) {
  if (!is.function(density)) {
    cli::cli_abort("Expected 'density' to be a function", arg = "density")
  }

  if (!is.null(fixed)) {
    assert_named(fixed, unique = TRUE)
    assert_list(fixed, call = call)
  }

  if (is.null(packer)) {
    packer <- monty_packer(
      setdiff(names(formals(density)), names(fixed)),
      fixed = fixed)
  } else {
    assert_is(packer, "monty_packer")
    if (!is.null(fixed)) {
      cli::cli_abort("Can't provide both 'packer' and 'fixed'", arg = "fixed")
    }
  }

  monty_model(
    list(parameters = packer$parameters,
         density = function(x) {
           rlang::inject(density(!!!packer$unpack(x)))
         }))
}
