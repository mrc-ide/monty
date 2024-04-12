##' Create a model using the mcstate2 DSL; this function will likely
##' change name in future, as will its interface.
##'
##' @title Domain Specific Language for mcstate
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
##' @return A [mcstate_model] object derived from the expressions you
##'   provide.
##'
##' @export
##' @examples
##'
##' # Expressions that correspond to models can be passed in with no
##' # quoting
##' mcstate_dsl(a ~ Normal(0, 1))
##' mcstate_dsl({
##'   a ~ Normal(0, 1)
##'   b ~ Exponential(1)
##' })
##'
##' # You can also pass strings
##' mcstate_dsl("a ~ Normal(0, 1)")
mcstate_dsl <- function(x, type = NULL) {
  quo <- rlang::enquo(x)
  if (rlang::quo_is_symbol(quo)) {
    x <- rlang::eval_tidy(quo)
  } else {
    x <- rlang::quo_get_expr(quo)
  }
  exprs <- dsl_preprocess(x, type)
  dat <- dsl_parse(exprs)
  NULL
}


mcstate_dsl_parse <- function(x, type = NULL) {
  quo <- rlang::enquo(x)
  if (rlang::quo_is_symbol(quo)) {
    x <- rlang::eval_tidy(quo)
  } else {
    x <- rlang::quo_get_expr(quo)
  }
  exprs <- dsl_preprocess(x, type)
  dsl_parse(exprs)
}
