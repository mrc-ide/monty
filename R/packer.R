##' Build a parameter packer, which can be used in models to translate
##' between an unstructured vector of numbers (the vector being
##' updated by an MCMC for example) to a structured list of named
##' values, which is easier to program against.  We refer to the
##' process of taking a named list of scalars, vectors and arrays and
##' converting into a single vector "packing" and the inverse
##' "unpacking".
##'
##' There are several places where it is most convenient to work in an
##' unstructured vector:
##'
##' * An MCMC is typically discussed as a the updating of some
##'   vector `x` to another `x'`
##' * An optimisation algorithm will try and find a set of values for
##'   a vector `x` that minimises (or maximises) some function `f(x)`
##' * An ode solver works with a vector `x(t)` (`x` at time `t`) and
##'   considers `x(t + h)` by computing the vector of derivatives
##'   `dx(t)/dt`
##'
##' In all these cases, the algorithm that needs the vector of numbers
##' knows nothing about what they represent.  Commonly, these will be
##' a packed vector of parameters.  So our vector `x` might actually
##' represent the parameters `a`, `b` and `c` in a vector as `[a, b,
##' c]` - this is a very common pattern, and you have probably
##' implemented this yourself.
##'
##' In more complex settings, we might want our vector `x` to collect
##' more structured quantities.  Suppose that you are fitting a model
##' with an age-structured or sex-structured parameter.  Rather than
##' having a series of scalars packed into your vector `x` you might
##' have a series of values destined to be treated as a vector:
##'
##' ```
##' | 1  2  3  4  5  6  7  |
##' | a  b  c  d1 d2 d3 d4 |
##' ```
##'
##' So here we might have a vector of length 7, where the first three
##' elements will represent be the scalar values `a`, `b` and `c` but
##' ther next four will be a vector `d`.
##'
##' Unpacked, this might be written as:
##'
##' ```
##' list(a = 1, b = 2, c = 3, d = 4:7)
##' ```
##'
##' The machinery here is designed to make these transformations
##' simple and standardised within mcstate2, and should be flexible
##' enough for many situations.  We will also use these from within
##' `dust2` and `odin2` for transformations in and out of vectors of
##' ODE state.
##'
##' # When to use `process`
##'
##' The `process` function is a get-out-of-jail function designed to
##' let you do arbitrary transformations when unpacking a vector.  In
##' general, this should not be the first choice to use because it is
##' less easy to reason about by other tooling (for example, as we
##' develop automatic diferentiation support for use with the HMC
##' algorithm, a `process` function will be problematic because we
##' will need to make sure we can differentiate this process).
##' However, there are cases where it will be only way to achieve some
##' results.
##'
##' Imagine that you are packing a 2x2 covariance matrix into your
##' vector in order to use within an MCMC or optimisation algorithm.
##' Ultimately, our unpacked vector will need to hold four elements
##' (`b11`, `b12`, `b21`, `b22`), but there are only three distinct
##' values as the two off-diagonal elements will be the same (i.e.,
##' `b12 == b21``).  So we might write this passing in `b_raw = 3` to
##' `array`, so that our unpacked list holds `b_raw = c(b11, b12,
##' b22)`.  We would then write `process` as something like:
##'
##' ```
##' process <- function(x) {
##'   list(b = matrix(x$b_raw[c(1, 2, 2, 3)], 2, 2))
##' }
##' ```
##'
##' which creates the symmetric 2x2 matrix `b` from `b_raw`.
##'
##' @title Build a parameter packer
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
##'   additional pieces of data that your model needs to run, but
##'   which you are not performing inference on.
##'
##' @param process An arbitrary R function that will be passed the
##'   final assembled parameter list; it may create any *additional*
##'   entries, which will be concatenated onto the original list.  If
##'   you use this you should take care not to return any values with
##'   the same names as entries listed in `scalar`, `array` or
##'   `fixed`, as this is an error (this is so that `pack()` is
##'   not broken).  We will likely play around with this process in
##'   future in order to get automatic differentiation to work.
##'
##' @return An object of class `mcstate_packer`, which has three
##'   elements:
##'
##' * `parameters`: a character vector of computed parameter names;
##'   these are the names that your statistical model will use.
##' * `unpack`: a function that can unpack an unstructured vector
##'   (say, from your statistical model parameters) into a structured
##'   list (say, for your generative model)
##' * `pack`: a function that can pack your structured list of
##'   parameters back into a numeric vector suitable for the
##'   statistical model.  This ignores values created by a
##'   `preprocess` function.
##'
##' @export
mcstate_packer <- function(scalar = NULL, array = NULL, fixed = NULL,
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
      tmp <- prepare_pack_array(nm, array[[nm]], call)
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
      c("Trying to generate an empty packer",
        i = paste("You have not provided any entries in 'scalar' or 'array',",
                  "which implies generating from a zero-length parameter",
                  "vector")))
  }

  if (!is.null(process)) {
    if (!is.function(process)) {
      cli::cli_abort(
        "Expected a function for 'process'",
        arg = "process")
    }
  }

  unpack <- function(x) {
    if (!is.null(names(x))) {
      if (!identical(names(x), parameters)) {
        ## Here, we could do better I think with this message; we
        ## might pass thropuigh empty names, and produce some summary
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
      dim2(res[[nm]]) <- shape[[nm]]
    }
    if (!is.null(fixed)) {
      res <- c(res, fixed)
    }
    if (!is.null(process)) {
      extra <- process(res)
      err <- intersect(names(extra), names(res))
      if (length(err) > 0) {
        cli::cli_abort(
          c("'process()' is trying to overwrite entries in parameters",
            i = paste("The 'process()' function should only create elemements",
                      "that are not already present in 'scalar', 'array'",
                      "or 'fixed', as this lets us reverse the transformation",
                      "process"),
            x = "{?Entry/Entries} already present: {squote(err)}"))
      }
      ## TODO: check names?
      ##
      ## TODO: this fails the multi-region use - but I think that we
      ## might want a different interface there anyway as we'll
      ## struggle to hold all the options here.
      res <- c(res, extra)
    }
    res
  }

  pack <- function(p) {
    res <- numeric(length(parameters))
    if (!all(lengths(p[names(idx)]) == lengths(idx))) {
      ## Not quite enough, because we should check the dimensions too.
      ## That ends up being quite hard with integer checks possibly
      ## because we really want to use identical() - this will do for now.
      cli::cli_abort("Invalid structure to pack()")
    }
    unlist(lapply(names(idx), function(el) p[el]), TRUE, FALSE)
  }

  ret <- list(parameters = parameters,
              unpack = unpack,
              pack = pack)
  class(ret) <- "mcstate_packer"
  ret
}


## Helper function to create array bookkeeping for the
## unpacking/packing process.
prepare_pack_array <- function(name, shape, call = NULL) {
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

  ## Whole bunch of bookkeeping used above;

  ## * names contains generated parameter names with index accessors -
  ##   these will render nicely though they're gross in their own way.
  ## * shape contains generated dimensions of a resulting array
  ## * n is the total number of parameters after this expansion
  list(names = sprintf("%s[%s]", name, index),
       shape = shape,
       n = length(index))
}


array_indices <- function(shape) {
  unname(as.matrix(do.call(expand.grid, lapply(shape, seq_len))))
}
