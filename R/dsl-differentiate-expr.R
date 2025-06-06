##' Differentiate expressions in the monty DSL.  This function is
##' exported for advanced use, and really so that we can use it from
##' odin.  But it has the potential to be generally useful, so while
##' we'll tweak the interface quite a lot over the next while it is
##' fine to use if you can handle some disruption.
##'
##' R already has support for differentiating expressions using [D],
##' which is useful for creating derivatives of simple functions to
##' pass into non-linear optimisation.  We need something a bit more
##' flexible for differentiating models in the monty DSL
##' ([monty_dsl]) and also in the related odin DSL.
##'
##' # Differences to [D()]
##'
##' * We try a little harder to simplify expressions.
##'
##' * The distribution functions in the monty DSL (e.g., Poisson)
##'   are (will be) handled specially, allowing substitution of
##'   log-densities and expectations.
##'
##' * Once we support array expressions, we will be able to
##'   differentiate through these.
##'
##' # Roadmap
##'
##' We may need to make this slightly extensible in future, but for
##' now the set of functions that can be differentiated is closed.
##'
##' # Warning
##'
##' The way of accessing distribution support here is peculiar and the
##' return type unusual.  This is intentional, and we expect a more
##' conventional interface in the future once this package settles
##' down.
##'
##' @title Differentiate expressions
##'
##' @return A list of related objects:
##'
##' * `differentiate`: A function that can differentiate an expression
##'   with respect to a variable (as a string).
##'
##' * `maths`: Some mathematical utilities for constructing
##'   expressions.  This will be documented later, but the most useful
##'   bits on here are the function elements `times`, `plus` and
##'   `plus_fold`.
##'
##' We will expand this soon to advertise what functions we are able
##' to differentiate to allow programs to fail fast.
##'
##' @export
##' @examples
##' d <- monty_differentiation()
##' d$differentiate(quote(sqrt(sin(x))), "x")
##' D(quote(sqrt(sin(x))), "x")
monty_differentiation <- function() {
  ## TODO: we might want to export a stripped down copy of maths
  ## perhaps.
  list(differentiate = differentiate,
       maths = maths)
}


## Differentiation support for expressions in the monty and odin
## DSLs (and not for R generally). This will handle the same array and
## summation semantics as used in odin, as well as differentiation of
## stochastic processes made deterministic.
##
## There are two levels here;
##
## 1. differentiate, via the list of functions "derivative" does the
##    differentiation of each expression
## 2. expression writing, via the environment of functions "maths"
##    does a moderately sensible job of writing expressions with a bit
##    of simplification.
##
## The maths step can be entirely removed but you do end up with lots
## of really ugly expressions with extra parentheses and things
## multiplied by zero that should be removed.
differentiate <- function(expr, name) {
  assert_scalar_character(name)
  if (is.numeric(expr)) {
    0
  } else if (is.symbol(expr)) {
    if (identical(expr, as.symbol(name))) 1 else 0
  } else {
    fn <- as.character(expr[[1]])
    if (!fn %in% names(derivative)) {
      cli::cli_abort(
        "Unsupported function '{fn}' in 'differentiate()'",
        class = "monty_differentiation_failure")
    }
    derivative[[fn]](expr, name)
  }
}


derivative <- list(
  `+` = function(expr, name) {
    maths$plus(differentiate(expr[[2]], name),
               differentiate(expr[[3]], name))
  },
  `-` = function(expr, name) {
    if (length(expr) == 3) {
      maths$minus(differentiate(expr[[2]], name),
                  differentiate(expr[[3]], name))
    } else {
      maths$uminus(differentiate(expr[[2]], name))
    }
  },
  `*` = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    b <- maths$rewrite(expr[[3]])
    da <- differentiate(a, name)
    db <- differentiate(b, name)
    maths$plus(maths$times(da, b), maths$times(a, db))
  },
  `/` = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    b <- maths$rewrite(expr[[3]])
    da <- differentiate(a, name)
    db <- differentiate(b, name)
    maths$minus(
      maths$divide(da, b),
      maths$divide(maths$times(a, db), maths$times(b, b)))
  },
  `^` = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    b <- maths$rewrite(expr[[3]])
    da <- differentiate(a, name)
    db <- differentiate(b, name)
    if (maths$is_zero(db)) {
      maths$times(b, maths$times(maths$pow(a, maths$minus(b, 1)), da))
    } else if (maths$is_zero(da)) {
      maths$times(maths$pow(a, b), maths$times(call("log", a), db))
    } else {
      ## a^(b - 1) * (b da + a log(a) db)
      maths$times(
        maths$pow(a, maths$minus(b, 1)),
        maths$plus(maths$times(b, da),
                   maths$times(a, maths$times(call("log", a), db))))
    }
  },
  `(` = function(expr, name) {
    differentiate(expr[[2]], name)
  },
  `[` = function(expr, name) {
    target <- as.character(expr[[2]])
    if (target != name) {
      return(0)
    }
    index <- as.list(expr[-(1:2)])

    ## Assume odin's indexes for now
    idx <- c("i", "j", "k", "l", "i5", "i6", "i7", "i8")[seq_along(index)]
    i <- Map(maths$is_same, index, lapply(idx, as.name))

    if (any(vlapply(i, isFALSE))) {
      return(0)
    }
    j <- vlapply(i, isTRUE)
    if (all(j)) {
      return(1)
    }

    ## Have to resort to some actual calculation here, sadly:
    call("if", maths$fold("&&", i[!j]), 1, 0)
   },
  exp = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$times(differentiate(a, name), call("exp", a))
  },
  log = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$divide(differentiate(a, name), a)
  },
  expm1 = function(expr, name) {
    ## exp(x) - 1, so this is the same as exp(x)
    a <- maths$rewrite(expr[[2]])
    maths$times(differentiate(a, name), call("exp", a))
  },
  log1p = function(expr, name) {
    ## > log(1 + x)
    a <- maths$rewrite(expr[[2]])
    maths$divide(differentiate(a, name), maths$plus(1, a))
  },
  log2 = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$divide(differentiate(a, name), maths$times(a, log(2)))
  },
  log10 = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$divide(differentiate(a, name), maths$times(a, log(10)))
  },
  sqrt = function(expr, name) {
    maths$divide(differentiate(expr[[2]], name),
                 maths$times(2, maths$rewrite(expr)))
  },
  `if` = function(expr, name) {
    condition <- maths$rewrite(expr[[2]])
    da <- differentiate(expr[[3]], name)
    db <- differentiate(expr[[4]], name)
    if (identical(da, db)) da else call("if", condition, da, db)
  },
  min = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    b <- maths$rewrite(expr[[3]])
    call("if", call("<", a, b), differentiate(a, name), differentiate(b, name))
  },
  max = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    b <- maths$rewrite(expr[[3]])
    call("if", call(">=", a, b), differentiate(a, name), differentiate(b, name))
  },
  lfactorial = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$times(differentiate(a, name), call("digamma", maths$plus(a, 1)))
  },
  sin = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$times(differentiate(a, name), call("cos", a))
  },
  cos = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$uminus(maths$times(differentiate(a, name), call("sin", a)))
  },
  tan = function(expr, name) {
    ## d/dx tan(x) = sec^2(x) = 1/cos^2(x)
    a <- maths$rewrite(expr[[2]])
    maths$divide(differentiate(a, name), call("^", call("cos", a), 2))
  },
  abs = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$times(differentiate(a, name), call("sign", a))
  },
  lbeta = function(expr, name) {
    ## Consider lbeta(a, b) as lgamma(a) + lgamma(b) - lgamma(a + b)
    ##
    ## We rewrite the expression into this form and then differentiate
    ## it:
    a <- maths$rewrite(expr[[2]])
    b <- maths$rewrite(expr[[3]])
    expr2 <- maths$minus(
      maths$plus(call("lgamma", a), call("lgamma", b)),
      call("lgamma", maths$plus(a, b)))
    differentiate(expr2, name)
  },
  lgamma = function(expr, name) {
    a <- maths$rewrite(expr[[2]])
    maths$times(differentiate(a, name), call("digamma", a))
  },
  lchoose = function(expr, name) {
    ## This case exists only so that we can differentiate binomial
    ## likelihoods, it may not always be a reasonable thing to do,
    ## because the derivative is only defined with respect to 'n' not
    ## 'k' the way that the density is defined in R.
    ##
    ## We can consider this as:
    ## > lfactorial(n) - lfactorial(k) - lfactorial(n - k)
    ## > ^--a            ^--b            ^--c
    ##
    ## and then apply the chain rule as:
    n <- maths$rewrite(expr[[2]])
    k <- maths$rewrite(expr[[3]])
    a <- differentiate(call("lfactorial", n), name)
    b <- differentiate(call("lfactorial", k), name)
    c <- differentiate(call("lfactorial", maths$minus(n, k)), name)
    maths$minus(maths$minus(a, b), c)
  },
  sum = function(expr, name) {
    target <- expr[[2]]
    if (is.symbol(target)) {
      return(if (as.character(target) == name) 1 else 0)
    }
    stopifnot(rlang::is_call(target, "["))

    index <- as.list(target[-(1:2)])
    target <- target[[2]]
    if (as.character(target) != name) {
      return(0)
    }

    ## Assuming that this is all "reasonable" following odin:
    index_full <- vlapply(index, rlang::is_missing)
    index_range <- vlapply(index, function(i) ":" %in% all.names(i))
    index_at <- !(index_full | index_range)

    idx <- c("i", "j", "k", "l", "i5", "i6", "i7", "i8")[seq_along(index)]

    i <- Map(maths$is_same, index[index_at], lapply(idx[index_at], as.name))
    if (any(vlapply(i, isFALSE))) {
      return(0)
    }

    if (any(index_range)) {
      i <- c(
        i,
        Map(function(a, b) call(">=", a, b),
            lapply(idx[index_range], as.name),
            lapply(index[index_range], function(e) e[[2]])),
        Map(function(a, b) call("<=", a, b),
            lapply(idx[index_range], as.name),
            lapply(index[index_range], function(e) e[[3]])))
    }

    j <- vlapply(i, isTRUE)
    if (all(j)) {
      return(1)
    }

    call("if", maths$fold("&&", i[!j]), 1, 0)
  }
)

maths <- local({
  .parentheses_except <- function(x, except) {
    if (is.recursive(x)) {
      fn <- as.character(x[[1]])
      if (fn == "(") {
        return(.parentheses_except(x[[2]], except))
      }
      pass <- grepl("^[A-Za-z]", fn) ||
        fn == "[" ||
        (length(except) > 0 && fn %in% except) ||
        "unary_minus" %in% except && .is_unary_minus(x)
      if (fn != "if" && pass) {
        return(x)
      }
      call("(", x)
    } else {
      x
    }
  }
  .drop_parens <- function(x) {
    if (rlang::is_call(x, "(")) .drop_parens(x[[2]]) else x
  }
  .is_unary_minus <- function(expr, recurse = FALSE) {
    (is.numeric(expr) && expr < 0) ||
      (rlang::is_call(expr, "-") && length(expr) == 2) ||
      (recurse && (
        (rlang::is_call(expr, "*") || rlang::is_call(expr, "/")) &&
        .is_unary_minus(expr[[2]], TRUE)))
  }
  is_zero <- function(x) {
    is.numeric(x) && x == 0
  }
  is_one <- function(x) {
    is.numeric(x) && x == 1
  }
  is_minus_one <- function(x) {
    is.numeric(x) && x == -1
  }
  plus <- function(a, b) {
    ## there's more cancelling here that could be done with
    ## expressions that involve subtraction and numbers, but I don't
    ## see them turning up anywhere yet; probably best to implement
    ## after we have them.
    if (is.numeric(a) && is.numeric(b)) {
      a + b
    } else if (is.numeric(b)) {
      plus(b, a)
    } else if (is_zero(a)) {
      .drop_parens(b)
    } else if (rlang::is_call(b, "+")) {
      plus(plus(a, b[[2]]), b[[3]])
    } else {
      call("+", a, b)
    }
  }
  minus <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a - b
    } else if (is_zero(b)) {
      .drop_parens(a)
    } else if (is_zero(a)) {
      uminus(b)
    } else {
      call("-", a, .parentheses_except(b, c("*", "/", "^")))
    }
  }
  uminus <- function(a) {
    if (is.numeric(a)) {
      -a
    } else if (.is_unary_minus(a)) {
      a[[2]]
    } else if (rlang::is_call(a, "*")) {
      if (.is_unary_minus(a[[2]])) {
        times(a[[2]][[2]], a[[3]])
      } else if (.is_unary_minus(a[[3]])) {
        times(a[[2]], a[[3]][[2]])
      } else {
        times(uminus(a[[2]]), a[[3]])
      }
    } else if (rlang::is_call(a, "/")) {
      divide(uminus(a[[2]]), a[[3]])
    } else if (rlang::is_call(a, "-") && length(a) == 3) {
      minus(a[[3]], a[[2]])
    } else if (rlang::is_call(a, "(")) {
      uminus(.drop_parens(a))
    } else {
      call("-", .parentheses_except(a, c("*", "/", "^")))
    }
  }
  times <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a * b
    } else if (is.numeric(b)) {
      ## below here, we can assume that 'b' is a language expression
      times(b, a)
    } else if (is_zero(a)) {
      0
    } else if (is_one(a)) {
      rewrite(b)
    } else if (is_minus_one(a)) {
      uminus(b)
    } else if (rlang::is_call(a, "/")) {
      ## we have (a2 / a3 * b -> a2 * b / a3)
      divide(times(a[[2]], b), a[[3]])
    } else if (rlang::is_call(b, "/")) {
      ## we have (a * (b2 / b3)) -> (a * b2) / b3
      divide(times(a, b[[2]]), b[[3]])
    } else if (rlang::is_call(b, "*") && is.numeric(b[[2]])) {
      times(times(a, b[[2]]), b[[3]])
    } else {
      if (.is_unary_minus(b, TRUE)) {
        ## We have expr * -b which we can write as -expr * b which
        ## simplifies more nicely. If we really had -a * -b this then
        ## becomes a * b which is nice.
        a <- uminus(a)
        b <- uminus(b)
      }
      aa <- .parentheses_except(a, c("*", "unary_minus", "/", "^"))
      bb <- .parentheses_except(b, c("*", "^"))
      if (rlang::is_call(bb, "*")) {
        call("*", call("*", aa, bb[[2]]), bb[[3]])
      } else {
        call("*", aa, bb)
      }
    }
  }
  divide <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a / b
    } else if (is_one(b)) {
      a
    } else if (is_zero(a)) {
      0
    } else if (identical(a, b)) {
      1
    } else if (rlang::is_call(a, "/")) {
      divide(a[[2]], times(a[[3]], b))
    } else if (rlang::is_call(b, "/")) {
      times(a, divide(b[[3]], b[[2]]))
    } else {
      if (.is_unary_minus(b, TRUE)) {
        a <- uminus(a)
        b <- uminus(b)
      }
      call("/",
           .parentheses_except(a, c("*", "unary_minus", "^")),
           .parentheses_except(b, "^"))
    }
  }
  pow <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      a^b
    } else if (is_one(b)) {
      a
    } else if (is_zero(b)) {
      1
    } else if (is.numeric(b) && b == 2 && is.symbol(a)) {
      times(a, a)
    } else {
      call("^", .parentheses_except(a, NULL), .parentheses_except(b, NULL))
    }
  }
  plus_fold <- function(x) {
    x <- x[!vlapply(x, is.null)]
    if (length(x) == 0) {
      0
    } else if (length(x) == 1) {
      x[[1]]
    } else {
      ret <- x[[1]]
      for (el in x[-1]) {
        ret <- plus(ret, el)
      }
      ret
    }
  }
  fold <- function(fn, x) {
    stopifnot(length(x) > 0)
    if (length(x) == 1) {
      x[[1]]
    } else {
      ret <- x[[1]]
      for (el in x[-1]) {
        ret <- call(fn, ret, el)
      }
      ret
    }
  }
  as_sum_of_parts <- function(expr) {
    if (rlang::is_call(expr, c("-", "+"), 2)) {
      if (rlang::is_call(expr, "-")) {
        parts <- lapply(expr[-1], as_sum_of_parts)
        uminus <- monty::monty_differentiation()$uminus
        parts[[2]] <- lapply(parts[[2]], uminus)
        unlist(parts, FALSE)
      } else {
        unlist(lapply(expr[-1], as_sum_of_parts), FALSE)
      }
    } else {
      list(expr)
    }
  }
  factorise_parts <- function(parts) {
    f <- function(el) {
      if (is.numeric(el)) {
        list(el, 1, "")
      } else if (rlang::is_call(el, "-", 1)) {
        ret <- f(el[[2]])
        ret[[1]] <- -1 * ret[[1]]
        ret
      } else {
        list(1, el, rlang::hash(el))
      }
    }
    parts <- lapply(parts, f)
    id <- vcapply(parts, "[[", 3)
    ret <- lapply(unname(split(parts, id)), function(el) {
      n <- sum(vnapply(el, "[[", 1))
      times(n, el[[1]][[2]])
    })
    plus_fold(ret)
  }
  factorise <- function(x) {
    factorise_parts(as_sum_of_parts(x))
  }
  is_same <- function(a, b) {
    if (is.numeric(a) && is.numeric(b)) {
      return(a == b)
    }
    if (identical(a, b)) {
      return(TRUE)
    }
    if (!is.recursive(a) && !is.recursive(b)) {
      return(call("==", a, b))
    }

    a_parts <- as_sum_of_parts(a)
    b_parts <- lapply(as_sum_of_parts(b), uminus)
    ab <- factorise_parts(c(a_parts, b_parts))

    if (is.numeric(ab)) {
      return(ab == 0)
    }

    call("==", ab, 0)
  }
  rewrite <- function(expr) {
    if (is.recursive(expr)) {
      fn <- as.character(expr[[1]])
      args <- lapply(expr[-1], rewrite)
      if (fn == "+") {
        plus(args[[1]], args[[2]])
      } else if (fn == "-" && length(expr) == 3) {
        minus(args[[1]], args[[2]])
      } else if (fn == "-" && length(expr) == 2) {
        uminus(args[[1]])
      } else if (fn == "*") {
        times(args[[1]], args[[2]])
      } else if (fn == "/") {
        divide(args[[1]], args[[2]])
      } else if (fn == "*") {
        times(args[[1]], args[[2]])
      } else if (fn == "^") {
        pow(args[[1]], args[[2]])
      } else if (fn == "(") {
        args[[1]]
      } else {
        as.call(c(list(expr[[1]]), args))
      }
    } else {
      expr
    }
  }
  as.list(environment())
})
