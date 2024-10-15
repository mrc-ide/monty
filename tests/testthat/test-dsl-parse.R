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
      b <- Normal(a, 1)
      a <- Normal(0, 1)
    }),
    "Invalid use of variable 'a'")
  expect_equal(res$src, quote(b <- Normal(a, 1)))
  expect_equal(res$context,
               list("'a' is defined later:" = quote(a <- Normal(0, 1))))
})


test_that("variables must be defined somewhere", {
  res <- expect_error(
    monty_dsl_parse({
      a <- Normal(0, 1)
      b <- Normal(a, sd)
    }),
    "Invalid use of variable 'sd'")
  expect_equal(res$src, quote(b <- Normal(a, sd)))
})


test_that("require that stochastic relationships assign to a symbol", {
  expect_error(
    dsl_parse_expr_stochastic(quote(f(a) ~ Normal(0, 1))),
    "Expected lhs of '~' relationship to be a symbol")
  expect_error(
    dsl_parse_expr_stochastic(quote(1 ~ Normal(0, 1))),
    "Expected lhs of '~' relationship to be a symbol")
})


test_that("require that stochastic relationships use known distributions", {
  expect_error(
    dsl_parse_expr_stochastic(quote(a ~ f(0, 1))),
    "Unknown distribution 'f'",
    fixed = TRUE)
})


test_that("collect all dependncies in rhs", {
  res <- dsl_parse_expr_stochastic(quote(a ~ Normal(a + b * c, a / d)))
  expect_setequal(res$depends, c("a", "b", "c", "d"))
  res <- dsl_parse_expr_stochastic(quote(a ~ Normal(f(a), g(10))))
  expect_equal(res$depends, "a")
  res <- dsl_parse_expr_stochastic(quote(a ~ Normal(0, 1)))
  expect_equal(res$depends, character())
})


test_that("require that assignments assign to a symbol", {
  expect_error(
    dsl_parse_expr_assignment(quote(f(a) <- 10)),
    "Expected lhs of assignment to be a symbol")
  expect_error(
    dsl_parse_expr_assignment(quote(1 <- 10)),
    "Expected lhs of assignment to be a symbol")
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
