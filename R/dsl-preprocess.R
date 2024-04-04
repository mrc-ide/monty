## This is essentially the same basic logic as in odin_preprocess
## which has generally worked ok for us.  What we want is to have
## something nice to echo back to the user at the point of an error.
## This unfortunately means working with R's source reference types
## which are pretty terrible.  In the long term it would be nice to be
## able to point at individual errors _within_ the tree (so specific
## arguments etc) but that's quite hard so for now we will just hold a
## deparsed expression as an attribute of each expression.
dsl_preprocess <- function(x, type = NULL) {
  type <- preprocess_detect(x, type)
  if (type == "expression") {
    if (inherits(x, "{")) {
      ret <- as.list(x[-1L])
    } else {
      ret <- list(x)
    }
  } else {
    if (type == "file") {
      exprs <- parse(file = x, keep.source = TRUE)
    } else {
      exprs <- parse(text = x, keep.source = TRUE)
    }
    expr_line <- utils::getSrcLocation(exprs, "line", first = TRUE)
    expr_str <- lapply(utils::getSrcref(exprs), as.character)

    ## Convert each expression to hold its starting line and string
    ## representation
    ret <- Map(structure, exprs, line = expr_line, str = expr_str)
  }
  ret
}


preprocess_detect <- function(x, type, call = NULL) {
  has_type <- !is.null(type)
  if (has_type) {
    type <- match_value(type, c("file", "text", "expression"),
                        call = call)
  }
  if (is.language(x)) {
    if (has_type && type != "expression") {
      cli::cli_abort("Invalid input for 'x': expected {type}", call = call)
    }
    as <- "expression"
  } else if (is.character(x)) {
    if (has_type) {
      if (type == "expression") {
        cli::cli_abort("Invalid input for 'x': expected expression",
                       call = call)
      } else if (type == "file") {
        assert_scalar_character(x, call = call)
        if (!file.exists(x)) {
          cli::cli_abort("File '{x}' does not exist", call = call)
        }
      }
      as <- type
    } else if (length(x) != 1L || grepl("([\n;=]|<-|.~)", x)) {
      as <- "text"
    } else if (file.exists(x)) {
      as <- "file"
    } else {
      cli::cli_abort("'{x}' looks like a filename, but file does not exist",
                     call = call)
    }
  } else {
    cli::cli_abort("Invalid input for 'x'")
  }
  as
}
