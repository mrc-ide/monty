test_that("can throw sensible errors with expression information", {
  expr <- structure(quote(a + b), line = 10, str = "a+b")
  msg <- "some error message"
  err <- expect_error(dsl_parse_error(msg, "E001", expr), "some error message")
  expect_equal(err$src, expr)
  expect_match(cli::ansi_strip(conditionMessage(err)),
               "In expression\n 10| a+b", fixed = TRUE)
})


test_that("can throw sensible errors without expression information", {
  expr <- quote(a + b)
  msg <- "some error message"
  err <- expect_error(dsl_parse_error(msg, "E100", expr), "some error message")
  expect_equal(err$src, expr)
})


test_that("can parse trivial model", {
  res <- monty_dsl_parse(a ~ Normal(0, 1))
  expect_equal(res$parameters, "a")
  expect_length(res$exprs, 1)
  expect_equal(res$exprs,
               list(dsl_parse_expr_stochastic(quote(a ~ Normal(0, 1)))))
})


test_that("can parse two-parameter model", {
  res <- monty_dsl_parse({
    a ~ Normal(0, 1)
    b ~ Exponential(1)
  })
  expect_equal(res$parameters, c("a", "b"))
  expect_length(res$exprs, 2)
  expect_equal(res$exprs,
               list(dsl_parse_expr_stochastic(quote(a ~ Normal(0, 1))),
                    dsl_parse_expr_stochastic(quote(b ~ Exponential(1)))))
})


test_that("can parse model with expressions", {
  res <- monty_dsl_parse({
    sd <- sqrt(10)
    a ~ Normal(0, sd)
  })
  expect_equal(res$parameters, "a")
  expect_length(res$exprs, 2)
  expect_equal(res$exprs,
               list(dsl_parse_expr_assignment(quote(sd <- sqrt(10))),
                    dsl_parse_expr_stochastic(quote(a ~ Normal(0, sd)))))
})


test_that("prevent models that imply duplicated parameters", {
  res <- expect_error(
    monty_dsl_parse("a~Normal(0,1)\na  ~  Uniform( 0,  1 )"),
    "Duplicated relationship 'a'")
  expect_equal(res$src,
               structure(quote(a ~ Uniform(0, 1)),
                         line = 2, str = "a  ~  Uniform( 0,  1 )"))
  expect_equal(names(res$context), "Previous definition")
  expect_equal(res$context[[1]],
               structure(quote(a ~ Normal(0, 1)),
                         line = 1, str = "a~Normal(0,1)"))
})


test_that("prevent multiple assignment", {
  expect_error(
    dsl_parse(list(quote(a <- 1), quote(a <- 2))),
    "Duplicated assignment 'a'")
})


test_that("assignments and relationships must be distinct", {
  expect_error(
    dsl_parse(list(quote(a ~ Normal(0, 1)), quote(a <- 1))),
    "Assignment 'a' shadows previous relationship")
  expect_error(
    dsl_parse(list(quote(a <- 1), quote(a ~ Normal(0, 1)))),
    "Relationship 'a' shadows previous assignment")
})


test_that("variables are not used out of order", {
  res <- expect_error(
    monty_dsl_parse({
      b ~ Normal(a, 1)
      a ~ Normal(0, 1)
    }),
    "Invalid use of variable 'a'")
  expect_equal(res$src, quote(b ~ Normal(a, 1)))
  expect_equal(res$context,
               list("'a' is defined later:" = quote(a ~ Normal(0, 1))))
})


test_that("require that stochastic relationships assign to a symbol", {
  expect_error(
    dsl_parse_expr_stochastic(quote(f(a) ~ Normal(0, 1))),
    "Invalid special function 'f()' on the lhs of a `~` relationship", 
    fixed = TRUE)
  expect_error(
    dsl_parse_expr_stochastic(quote(1 ~ Normal(0, 1))),
    "Invalid target '1' on the lhs of a `~` relationship", fixed = TRUE)
  expect_error(
    dsl_parse_expr_stochastic(quote(f(a)[] ~ Normal(0, 1))),
    "Invalid special function 'f()' on the lhs of a `~` array relationship", 
    fixed = TRUE)
  expect_error(
    dsl_parse_expr_stochastic(quote(1[] ~ Normal(0, 1))),
    "Invalid target '1' on the lhs of a `~` array relationship", fixed = TRUE)
})


test_that("require that stochastic relationships use known distributions", {
  expect_error(
    dsl_parse_expr_stochastic(quote(a ~ f(0, 1))),
    "Unknown distribution 'f'",
    fixed = TRUE)
})


test_that("collect all dependencies in rhs", {
  res <- dsl_parse_expr_stochastic(quote(a ~ Normal(a + b * c, a / d)))
  expect_setequal(res$rhs$depends, c("a", "b", "c", "d"))
  res <- dsl_parse_expr_stochastic(quote(a ~ Normal(f(a), g(10))))
  expect_equal(res$rhs$depends, "a")
  res <- dsl_parse_expr_stochastic(quote(a ~ Normal(0, 1)))
  expect_equal(res$rhs$depends, character())
})


test_that("require that assignments assign to a symbol", {
  expect_error(
    dsl_parse_expr_assignment(quote(f(a) <- 10)),
    "Invalid special function 'f()' on the lhs of assignment",
    fixed = TRUE)
  expect_error(
    dsl_parse_expr_assignment(quote(1 <- 10)),
    "Invalid target '1' on the lhs of assignment",
    fixed = TRUE)
  expect_error(
    dsl_parse_expr_assignment(quote(f(a)[] <- 10)),
    "Invalid special function 'f()' on the lhs of array assignment",
    fixed = TRUE)
  expect_error(
    dsl_parse_expr_assignment(call("<-", call("[", 1), 10)),
    "Invalid target '1' on the lhs of array assignment",
    fixed = TRUE)
})


test_that("every expression is classifiable", {
  expect_error(
    dsl_parse_expr(1),
    "Unhandled expression; expected something involving '~' or '<-'",
    fixed = TRUE)
  expect_error(
    dsl_parse_expr(quote(a == 1)),
    "Unhandled expression; expected something involving '~' or '<-'",
    fixed = TRUE)
})


test_that("Can run high level dsl parse function", {
  res <- monty_dsl_parse("a ~ Normal(0, 1)")
  expect_equal(res$parameters, "a")
  x <- "a ~ Normal(0, 1)"
  expect_equal(monty_dsl_parse(x), res)
})


test_that("require that rhs to stochastic statement is a call", {
  err <- expect_error(
    monty_dsl_parse(a ~ 1),
    "rhs is not a function call")
})


test_that("require that rhs to stochastic statement is known distribution", {
  err <- expect_error(
    monty_dsl_parse(a ~ normal(0, 1)),
    "Unknown distribution 'normal'")
  expect_match(conditionMessage(err),
               "Did you mean: 'Normal'")
})


test_that("sensisible error if no suggestions for distribution found", {
  err <- expect_error(
    monty_dsl_parse(a ~ QQQ(0, 1)),
    "Unknown distribution 'QQQ'")
  expect_no_match(conditionMessage(err), "Did you mean:")
})


test_that("report back invalid distribution calls", {
  err <- expect_error(
    monty_dsl_parse(a ~ Normal(0, 1, 2)),
    "Invalid call to 'Normal()'", fixed = TRUE)
  expect_equal(
    err$body,
    c("x" = "Failed to match given arguments: <1>, <2>, <3>",
      "i" = "Call should match:",
      "*" = "mean, sd"))
})


test_that("can explain an error", {
  skip_if_not_installed("mockery")
  mock_explain <- mockery::mock()
  mockery::stub(monty_dsl_error_explain, "error_explain", mock_explain)
  monty_dsl_error_explain("E101")
  mockery::expect_called(mock_explain, 1)
  expect_equal(
    mockery::mock_args(mock_explain)[[1]],
    list(dsl_errors, "E101", "pretty"))
})


test_that("empty fixed data is null", {
  expect_null(check_dsl_fixed(NULL))
  expect_null(check_dsl_fixed(list()))
})


test_that("validate fixed data for dsl", {
  expect_error(
    check_dsl_fixed(c(a = 1, b = 2)),
    "Expected 'fixed' to be a list")
  expect_error(
    check_dsl_fixed(list(a = 1, b = 2, a = 2)),
    "'fixed' must have unique names")
  expect_error(
    check_dsl_fixed(list(a = 1, b = 2:3, c = numeric(10))),
    "All elements of 'fixed' must currently be scalars")
  expect_equal(
    check_dsl_fixed(list(a = 1, b = 2)),
    list(a = 1, b = 2))
})


test_that("assignments cannot shadow names of fixed variables", {
  expect_error(
    dsl_parse(list(quote(a <- 1)), fixed = list(a = 1)),
    "Value 'a' in 'fixed' is shadowed by assignment")
})


test_that("dim call on lhs requires variables", {
  expect_error(
    dsl_parse(list(quote(dim() <- 1))),
    "Invalid call to 'dim()' on lhs; no variables given",
    fixed = TRUE)
})


test_that("arguments to dim call on lhs must be unnamed", {
  expect_error(
    dsl_parse(list(quote(dim(x = a) <- 1))),
    "Invalid call to 'dim()' on lhs; arguments must be unnamed",
    fixed = TRUE)
})


test_that("arguments to dim call on lhs must be symbols", {
  expect_error(
    dsl_parse(list(quote(dim(f(x)) <- 1))),
    "Invalid call to 'dim()' on lhs; 'f(x)' is not a symbol",
    fixed = TRUE)
})


test_that("dim call on rhs only takes a symbol", {
  expect_error(
    dsl_parse(list(quote(dim(a) <- dim(b[])))),
    "When using 'dim()' on the right-hand-side, it takes only an array name",
    fixed = TRUE)
})


test_that("rhs of dim equation restricts use of functions", {
  expect_error(
    dsl_parse(list(quote(dim(a) <- f(1)))),
    "Invalid function used on rhs of 'dim()': 'f'",
    fixed = TRUE)
  expect_error(
    dsl_parse(list(quote(dim(a) <- f(1 * a)))),
    "Invalid functions used on rhs of 'dim()': 'f' and '*'",
    fixed = TRUE)
})


test_that("dimensions of a variable cannot be given multiple times", {
  expect_error(
    dsl_parse(list(quote(dim(x) <- 1), quote(dim(x) <- 2))),
    "The variable x was given dimensions multiple times",
    fixed = TRUE)
})


test_that("index access on rhs determined by dimensions on lhs", {
  expect_error(
    dsl_parse(list(quote(x[] <- j))),
    "Invalid index access used on rhs of equation: 'j'",
    fixed = TRUE)
  expect_error(
    dsl_parse(list(quote(x[, ] <- k))),
    "Invalid index access used on rhs of equation: 'k'",
    fixed = TRUE)
})


test_that("cannot use restricted names as lhs target", {
  expect_error(
    dsl_parse(list(quote(k <- 5))),
    "Can't assign to reserved name 'k'",
    fixed = TRUE)
  expect_error(
    dsl_parse(list(quote(j[] ~ Poisson(3)))),
    "Can't assign to reserved name 'j'",
    fixed = TRUE)
  expect_error(
    dsl_parse(list(quote(dim <- 1))),
    "Can't assign to reserved name 'dim'",
    fixed = TRUE)
  expect_error(
    dsl_parse(list(quote(pi <- 1))),
    "Do not use `pi` on the left-hand-side of an expression",
    fixed = TRUE)
})


test_that("only special functions allowed on lhs", {
  expect_error(
    dsl_parse(list(quote(dym(1) <- 1))),
    "Invalid special function 'dym()' on the lhs of assignment",
    fixed = TRUE)
  expect_error(
    dsl_parse(list(quote(exp(x) <- 1))),
    "Invalid special function 'exp()' on the lhs of assignment",
    fixed = TRUE)
})


test_that("arrays require a dim equation", {
  expect_error(
    dsl_parse(list(quote(x[] <- Exponential(1)))),
    "Missing 'dim()' for expression assigned as an array: 'x'",
    fixed = TRUE)
}) 


test_that("array equations require [] on the lhs", {
  expect_error(
    dsl_parse(list(quote(x[] ~ Exponential(1)), 
                   quote(x ~ Exponential(1)), 
                   quote(dim(x) <- 3))),
    "Array expressions must always use '[]' on the lhs",
    fixed = TRUE)
})


test_that("can alias dims", {
  res <- dsl_parse(
    list(quote(a[] <- exp(i)),
         quote(b[] ~ Normal(0, a[i])),
         quote(dim(a) <- 3),
         quote(dim(b) <- dim(a))),
    gradient_required = FALSE)
  expect_equal(res$arrays$alias[res$arrays$name == "b"], "a")
  expect_identical(res$arrays$dims[res$arrays$name == "b"], 
                   res$arrays$dims[res$arrays$name == "a"])
  
  res <- dsl_parse(
    list(quote(a[] <- exp(i)),
         quote(b[] ~ Normal(0, a[i])),
         quote(c[] ~ Exponential(3)),
         quote(dim(a) <- 3),
         quote(dim(b, c) <- dim(a))),
    gradient_required = FALSE)
  expect_equal(res$arrays$alias[res$arrays$name %in% c("b", "c")], c("a", "a"))
  expect_identical(res$arrays$dims[res$arrays$name == "b"], 
                   res$arrays$dims[res$arrays$name == "a"])
  expect_identical(res$arrays$dims[res$arrays$name == "c"], 
                   res$arrays$dims[res$arrays$name == "a"])
  
  res <- dsl_parse(
    list(quote(a[] <- exp(i)),
         quote(b[] ~ Normal(0, a[i])),
         quote(c[] ~ Exponential(3)),
         quote(dim(a) <- 3),
         quote(dim(b) <- dim(a)),
         quote(dim(c) <- dim(b))),
    gradient_required = FALSE)
  expect_equal(res$arrays$alias[res$arrays$name %in% c("b", "c")], c("a", "a"))
  expect_identical(res$arrays$dims[res$arrays$name == "b"], 
                   res$arrays$dims[res$arrays$name == "a"])
  expect_identical(res$arrays$dims[res$arrays$name == "c"], 
                   res$arrays$dims[res$arrays$name == "a"])
})


test_that("array equations cannot be interleaved", {
  expect_error(
    dsl_parse(list(quote(x[1] ~ Exponential(1)),
                   quote(y ~ Uniform(0, 1)),
                   quote(x[2:3] ~ Exponential(3)), 
                   quote(dim(x) <- 3))),
    paste("Multiline array equations must be contiguous statements,",
          "but 'x' is interleaved with 'y'"),
    fixed = TRUE)
})


test_that("lhs indexing for array equations is restricted", {
  expect_error(dsl_parse(list(quote(x[TRUE] ~ Exponential(1)))),
    "Invalid value for array index lhs",
    fixed = TRUE)
  
  expect_error(dsl_parse(list(quote(x[a:b + 1] ~ Exponential(1)))),
               "Invalid use of range operator ':' on lhs of array assignment",
               fixed = TRUE)
  
  expect_error(dsl_parse(list(quote(x[f(1)] ~ Exponential(1)))),
               "Invalid function used in lhs of array assignment: 'f'",
               fixed = TRUE)
  
  expect_error(dsl_parse(list(quote(x[-1] ~ Exponential(1)))),
               "Invalid use of unary minus in lhs of array assignment",
               fixed = TRUE)
  
  expect_error(dsl_parse(list(quote(x[i] ~ Exponential(1)))),
               paste("Invalid use of special variable in lhs of array",
                     "assignment: 'i'"),
               fixed = TRUE)
})


test_that("unknown variables result in an error", {
  expect_error(dsl_parse(list(quote(x ~ Exponential(a)))),
               "Unknown variable used: 'a'",
               fixed = TRUE)
  
  expect_error(dsl_parse(list(quote(x ~ Exponential(a * b)))),
               "Unknown variables used: 'a' and 'b'",
               fixed = TRUE)
  
  err <- expect_error(dsl_parse(list(quote(x ~ Exponential(aaa))),
                                fixed = list(aa = 1)),
                      "Unknown variable used: 'aaa'",
                      fixed = TRUE)
  expect_equal(err$body, c(i = "Did you mean 'aa'?"))
})
