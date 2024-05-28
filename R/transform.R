##' Build a parameter transformer, which can be used in models to
##' translate between an unstructured vector of numbers (the vector
##' being updated by an MCMC for example) to a structured list of
##' named values, which is easier to program against.
##'
##' @title Build a parameter transformer
##'
##' @param scalar Names of scalar parameters.  This is similar for
##'   listing elements in `array` with values of 1, though elements in
##'   `scalar` will be placed ahead of those listed in `array` within
##'   the final parameter vector, and elements in `array` will have
##'   generated names that include square brackets.
##'
##' @param array A list, where names correspond to the names of array
##'   parameters and values correspond to the lengths of parameters.
##'   Multiple dimensions are allowed (so if you provide an element
##'   with two entries these represent dimensions of a matrix).  In
##'   future, you may be able to use *strings* as values for the
##'   lengths, in which case these will be looked for within `fixed`.
##'
##' @param fixed A named list of fixed parameters; these will be added
##'   into the final list directly.  These typically represent
##'   additional pieces of data that your model needs to run, but for
##'   which you are not performing inference on.
##'
##' @param process An arbitrary R function that will be passed the
##'   final assembled parameter list and will process it - this
##'   function should return a named list too, but can do whatever
##'   processing you need.  The default is not to have any processing
##'   (equivalent to using [identity]).  If you use this, you should
##'   take care to only append onto your list, and not to modify any
##'   values - otherwise you will break the generated `untransform`
##'   method.  We will likely play around with this process in future
##'   in order to get automatic differentiation to work.
##'
##' @return An object of class `mcstate_transformer`, which has three
##'   elements:
##'
##' * `parameters`: a character vector of computed parameter names;
##'   these are the names that your statistical model will use.
##' * `transform`: a function that can transform from your statistical
##'   parameters into a structured list of parameters.
##' * `untransform`: a function that can untransform from your
##'   structured list of parameters back into a numeric vector
##'   suitable for the statistical model.  This cannot untransform
##'   through a `preprocess` function.
##'
##' @export
mcstate_transformer <- function(scalar = NULL,
                                array = NULL,
                                fixed = NULL,
                                process = NULL) {
  call <- environment()

  parameters <- character(0)
  idx <- list()
  shape <- list()

  if (!is.null(scalar)) {
    if (!is.character(scalar)) {
      cli::cli_abort(
        "Expected a character vector for 'scalar'",
        arg = "scalar")
    }
    dups <- duplicate_values(scalar)
    if (length(dups) > 0) {
      cli::cli_abort(
        c("Elements of 'scalar' must be unique",
          i = "Found {length(dups)} duplicate{?s}: {collapseq(dups)}"),
        arg = "scalar")
    }
    idx[scalar] <- set_names(as.list(seq_along(scalar)), scalar)
    parameters <- c(parameters, scalar)
  }

  len <- length(scalar) # start after scalars
  if (!is.null(array)) {
    assert_named(array, unique = TRUE, call = call)
    for (nm in names(array)) {
      tmp <- transform_array(nm, array[[nm]], call)
      parameters <- c(parameters, tmp$names)
      shape[[nm]] <- tmp$shape
      idx[[nm]] <- seq_len(tmp$n) + len
      len <- len + tmp$n
    }
  }

  if (!is.null(fixed)) {
    assert_named(fixed, unique = TRUE, call = call)
    assert_list(fixed, call = call)
  }

  err <- duplicate_values(c(scalar, names(array), names(fixed)))
  if (length(err)) {
    detail <- sprintf("'%s' appears in more than one place", err)
    cli::cli_abort(
      c("Names must be distinct between 'scalar', 'array' and 'fixed'",
        set_names(detail, "x")))
  }

  if (length(idx) == 0) {
    cli::cli_abort(
      c("Trying to generate an empty transformer",
        i = paste("You have not provided any entries in 'scalar' or 'array',",
                  "which implies generating from a zero-length parameter",
                  "vector")))
  }

  transform <- function(x) {
    if (!is.null(names(x))) {
      if (!identical(names(x), parameters)) {
        ## Here, we could do better I think with this message; we
        ## might pass thrtough empty names, and produce some summary
        ## of different names.  Something for later though.
        cli::cli_abort("Incorrect names in input")
      }
      names(x) <- NULL
    }
    if (length(x) != len) {
      cli::cli_abort(
        "Incorrect length input; expected {len} but given {length(x)}")
    }
    res <- lapply(idx, function(i) x[i])
    for (nm in names(shape)) {
      dim(res[[nm]]) <- shape[[nm]]
    }
    if (!is.null(fixed)) {
      res <- c(res, fixed)
    }
    if (!is.null(process)) {
      res <- process(res)
    }
    res
  }

  untransform <- function(p) {
    res <- numeric(length(parameters))
    if (!all(lengths(p[names(idx)]) == lengths(idx))) {
      ## Not quite enough, because we should check the dimensions too.
      ## That ends up being quite hard with integer checks possibly
      ## because we really want to use identical() - this will do for now.
      cli::cli_abort("Invalid structure to untransform")
    }
    unlist(lapply(names(idx), function(el) p[el]), TRUE, FALSE)
  }

  ret <- list(parameters = parameters,
              transform = transform,
              untransform = untransform)
  class(ret) <- "mcstate_transformer"
  ret
}


transform_array <- function(name, shape, call = NULL) {
  if (length(shape) == 0) {
    cli::cli_abort(
      paste("Elements of 'array' must have at least one element, but",
            "'{name}' has none"),
      arg = "array", call = call)
  }
  if (!rlang::is_integerish(shape)) {
    cli::cli_abort(
      paste("Elements of 'array' must be integer-like vectors, but",
            "'{name}' is not"),
      arg = "array", call = call)
  }
  if (any(shape <= 0)) {
    cli::cli_abort(
      paste("All dimensions in 'array' must be at least 1, but",
            "'{name}' violates this"),
      arg = "array", call = call)
  }
  if (length(shape) == 1) {
    index <- as.character(seq_len(shape))
  } else {
    index <- apply(array_indices(shape), 1, paste, collapse = ",")
  }

  list(names = sprintf("%s[%s]", name, index),
       shape = if (length(shape) > 1) shape else NULL,
       n = length(index))
}


array_indices <- function(shape) {
  ## unname(as.matrix(rev(do.call(expand.grid, lapply(rev(shape), seq_len)))))
  unname(as.matrix(do.call(expand.grid, lapply(shape, seq_len))))
}
