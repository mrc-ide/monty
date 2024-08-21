##' Explain error codes produced by monty.  This is a work in progress,
##' and we would like feedback on what is useful as we improve it.
##' The idea is that if you see an error you can link through to get
##' more information on what it means and how to resolve it.  The
##' current implementation of this will send you to the rendered
##' vignettes, but in the future we will arrange for offline rendering
##' too.
##'
##' @title Explain monty error
##'
##' @param code The error code, as a string, in the form `Exxx` (a
##'   capital "E" followed by three numbers)
##'
##' @return Nothing, this is called for its side effect only
##'
##' @export
mcstate_dsl_error_explain <- function(code) {
  ## See odin2 for the canonical implementation of this, we're just
  ## shadowing it here really as our dsl is much simpler.  Note that
  ## error codes from mcstate are three numbers long, whereas they are
  ## four long in odin so that it's easy (for us) to tell who the
  ## error belongs to.
  assert_scalar_character(code)
  if (!grepl("^E[0-9]{3}$", code)) {
    cli::cli_abort("Invalid code '{code}', should match 'Exxx'",
                   arg = "code")
  }
  txt <- dsl_errors[[code]]
  if (is.null(txt)) {
    cli::cli_abort(
      c("Error '{code}' is undocumented",
        i = paste("If you were directed here from an error message, please",
                  "let us know (e.g., file an issue or send us a message)")),
      arg = "code")
  }
  url <- sprintf(
    "https://mrc-ide.github.io/monty/articles/dsl-errors.html#%s",
    tolower(code))
  utils::browseURL(url)
}


dsl_parse_error <- function(msg, code, src, call, ...,
                            .envir = parent.frame()) {
  stopifnot(grepl("^E[0-9]{3}$", code))
  cli::cli_abort(msg,
                 class = "monty_parse_error",
                 code = code,
                 src = src,
                 call = call,
                 ...,
                 .envir = .envir)
}


##' @importFrom rlang cnd_footer
##' @export
cnd_footer.monty_parse_error <- function(cnd, ...) {
  detail <- c(">" = "In expression",
              format_error_src(cnd$src))
  for (i in seq_along(cnd$context)) {
    detail <- c(detail,
                "",
                i = names(cnd$context)[[i]],
                format_error_src(cnd$context[[i]]))
  }
  ## Annoyingly, there's no way of marking text as whitespace
  ## preserving within cli, so we need to do a substitution here for
  ## "nonbreaking space" which does ok.  We should also convert tabs
  ## to some number of spaces, probably.
  detail <- gsub(" ", "\u00a0", detail)


  code <- cnd$code
  ## See https://cli.r-lib.org/reference/links.html#click-to-run-code
  ## RStudio will only run code in namespaced form
  explain <- cli::format_inline(
    paste("For more information, run",
          "{.run monty::monty_dsl_error_explain(\"{code}\")}"))
  c(detail, i = explain)
}


## TODO: this is nicer than what we do in odin.  I wonder if it might
## make sense to try and have odin2 depend on the code here?
format_error_src <- function(src) {
  str <- attr(src, "str", exact = TRUE)
  if (is.null(str)) {
    detail <- deparse(src)
  } else {
    ## We can adjust the formatting here later, but this will
    ## hopefully be fairly nice for users.
    lines <- seq(attr(src, "line"), length.out = length(str))
    detail <- sprintf("%s| %s", cli::col_grey(format(lines, width = 3)), str)
  }
  detail
}
