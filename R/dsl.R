##' Create a model using the monty DSL; this function will likely
##' change name in future, as will its interface.
##'
##' @title Domain Specific Language for monty
##'
##' @param x The model as an expression.  This may be given as an
##'   expression, as a string, or as a path to a filename.  Typically,
##'   we'll do a reasonable job of working out what you've provided
##'   but use the `type` argument to disambiguate or force a
##'   particular interpretation.  The argument uses rlang's quosures
##'   to allow you to work with expressions directly; see examples for
##'   details.
##'
##' @param type Force interpretation of the type of expression given
##'   as `x`.  If given, valid options are `expression`, `text` or
##'   `file`.
##'
##' @param gradient Control gradient derivation.  If `NULL` (the
##'   default) we try and generate a gradient function for your model
##'   and warn if this is not possible.  If `FALSE`, then we do not
##'   attempt to construct a gradient function, which prevents a
##'   warning being generated if this is not possible.  If `TRUE`,
##'   then we will error if it is not possible to create a gradient
##'   function.
##'
##' @param fixed An optional list of values that can be used within
##'   the dsl code.  Anything you provide here is available for your
##'   calculations.  In the interest of future compatibility, we check
##'   currently that all elements are scalars.  In future this may
##'   become more flexible and allow passing environments, etc.  Once
##'   provided, these values cannot be changed without rebuilding the
##'   model; they are fixed data.  You might use these for
##'   hyperparameters that are fixed across a set of model runs, for
##'   example.
##'
##' @return A [monty_model] object derived from the expressions you
##'   provide.
##'
##' @export
##' @examples
##'
##' # Expressions that define models can be passed in with no quoting
##' monty_dsl(a ~ Normal(0, 1))
##' monty_dsl({
##'   a ~ Normal(0, 1)
##'   b ~ Exponential(1)
##' })
##'
##' # You can also pass strings
##' monty_dsl("a ~ Normal(0, 1)")
monty_dsl <- function(x, type = NULL, gradient = NULL, fixed = NULL) {
  quo <- rlang::enquo(x)
  if (rlang::quo_is_symbol(quo)) {
    x <- rlang::eval_tidy(quo)
  } else {
    x <- rlang::quo_get_expr(quo)
  }
  call <- environment()
  fixed <- check_dsl_fixed(fixed)
  exprs <- dsl_preprocess(x, type, call)
  dat <- dsl_parse(exprs, gradient, fixed, call)
  dsl_generate(dat)
}



monty_dsl_parse <- function(x, type = NULL, gradient = NULL, fixed = NULL) {
  call <- environment()
  quo <- rlang::enquo(x)
  if (rlang::quo_is_symbol(quo)) {
    x <- rlang::eval_tidy(quo)
  } else {
    x <- rlang::quo_get_expr(quo)
  }
  fixed <- check_dsl_fixed(data, call)
  exprs <- dsl_preprocess(x, type, call)
  dsl_parse(exprs, gradient, call)
}


##' Parse an expression as if it were a call to one of monty's
##' distribution functions (e.g., `Normal`, `Poisson`).  This will
##' fill in any defaults, disambiguate where multiple
##' parameterisations of the distribution are available, and provide
##' links through to the C++ API.  This function is designed for use
##' from other packages that use monty, and is unlikely to be
##' useful to most users.
##'
##' @title Parse distribution expression
##'
##' @param expr An expression
##'
##' @param name Name for the expression, used in constructing messages
##'   that you can use in errors.
##'
##' @return A list; the contents of this are subject to change.
##'   However you can (to a degree) rely on the following elements:
##'
##' * `name`: The name of the distribution (e.g., `Normal`).  This
##'   will be the same as the name of the function called in `expr`
##'
##' * `variant`: The name of the distribution variant, if more than
##'   one is supported.
##'
##' * `args`: The arguments that you provided, in position-matched
##'   order
##'
##' * `cpp`: The names of the C++ entrypoint to use.  This is a list
##'   with elements `density` and `sample` for the log-density and
##'   sampling functions, and `NULL` where these do not yet exist.
##'
##' Currently we also include:
##'
##' * `density`: A function to compute the log-density.  This will
##'   likely change once we support creation of differentiable models
##'   because we will want to do something with the arguments
##'   provided!
##'
##' * `sample`: A function to sample from the distribution, given (as
##'   a first argument) a rng object (see [monty_rng])
##'
##' @export
##' @examples
##' # A successful match
##' monty_dsl_parse_distribution(quote(Normal(0, 1)))
##'
##' # An unsuccessful match
##' monty_dsl_parse_distribution(quote(Normal()))
monty_dsl_parse_distribution <- function(expr, name = NULL) {
  ## Here, the user has not provided a call to anything, or a call to
  ## something that is not recognised as a distribution.  We throw the
  ## same error in both cases, but with different contextual
  ## information in order to fix the error.
  if (!rlang::is_call(expr)) {
    name <- name %||% deparse(substitute(expr))
    error <- cli::format_inline("{name} is not a function call")
    return(list(success = FALSE, error = error))
  }

  if (!rlang::is_call(expr, names(dsl_distributions))) {
    distr_name <- as.character(expr[[1]])
    error <- c(
      cli::format_inline("Unknown distribution '{distr_name}'"),
      i = paste("See {.run monty::monty_dsl_distributions} for details on",
                "supported distributions"))
    dym <- near_match(distr_name, names(dsl_distributions))
    if (length(dym) > 0) {
      error <- c(error, i = cli::format_inline("Did you mean: {squote(dym)}?"))
    }
    return(list(success = FALSE, error = error))
  }

  ## Next, match the arguments to the call, in order to
  distr_name <- as.character(expr[[1]])
  args <- as.list(expr[-1])
  candidates <- dsl_distributions[[distr_name]]
  match <- match_call(args, lapply(candidates, "[[", "args"))
  if (!match$success) {
    error <- c(cli::format_inline("Invalid call to '{distr_name}()'"),
               match$error)
    return(list(success = FALSE, error = error))
  }

  value <- candidates[[match$index]]
  value$args <- unname(args[match$args])
  list(success = TRUE,
       value = value)
}


check_dsl_fixed <- function(fixed, call) {
  if (is.null(fixed)) {
    return(NULL)
  }
  assert_list(fixed, call = call)
  if (length(fixed) == 0) {
    return(NULL)
  }
  assert_named(fixed, unique = TRUE, call = call)
  err <- lengths(fixed) != 1
  if (any(err)) {
    info <- sprintf("'%s' had length %d",
                    names(fixed)[err], lengths(fixed[err]))
    cli::cli_abort(
      c("All elements of 'fixed' must currently be scalars",
        set_names(info, "x")),
      arg = "fixed", call = call)
  }
  fixed
}
