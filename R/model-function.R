##' Create a [monty_model] from a function that computes density.
##' This allows use of any R function as a simple monty model.  If you
##' need advanced model features, then this interface may not suit you
##' and you may prefer to to use [monty_model] directly.
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
##' @return A [monty_model] object that computes log density with the
##'   provided `density` function, given a numeric vector argument
##'   representing all parameters.
##'
##' @export
monty_model_function <- function(density, packer = NULL) {
  if (!is.function(density)) {
    cli::cli_abort("Expected 'density' to be a function", arg = "density")
  }

  if (is.null(packer)) {
    packer <- monty_packer(names(formals(density)))
  } else {
    assert_is(packer, "monty_packer")
    ## TODO: check parameters are all in function?
  }

  monty_model(
    list(parameters = packer$parameters,
         density = function(x) {
           rlang::inject(density(!!!packer$unpack(x)))
         }))
}
