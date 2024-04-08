test_that("can throw sensible errors with expression information", {
  expr <- structure(quote(a + b), line = 10, str = "a+b")
  msg <- "some error message"
  err <- expect_error(dsl_parse_error(msg, expr), "some error message")
  expect_equal(err$mcstate2_expr, expr)
  expect_match(cli::ansi_strip(conditionMessage(err)),
               "10| a+b", fixed = TRUE)
})


test_that("can throw sensible errors without expression information", {
  expr <- quote(a + b)
  msg <- "some error message"
  err <- expect_error(dsl_parse_error(msg, expr), "some error message")
  expect_equal(err$mcstate2_expr, expr)
  expect_match(cli::ansi_strip(conditionMessage(err)),
               "a + b", fixed = TRUE)
})


test_that("can parse trivial model", {
  res <- mcstate_dsl_parse(a ~ Normal(0, 1))
  expect_equal(res$parameters, "a")
  expect_length(res$exprs, 1)
  expect_equal(res$exprs,
               list(dsl_parse_expr_stochastic(quote(a ~ Normal(0, 1)))))
})


test_that("can parse two-parameter model", {
  res <- mcstate_dsl_parse({
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
  res <- mcstate_dsl_parse({
    sd <- sqrt(pi)
    a ~ Normal(0, sd)
  })
  expect_equal(res$parameters, "a")
  expect_length(res$exprs, 2)
  expect_equal(res$exprs,
               list(dsl_parse_expr_assignment(quote(sd <- sqrt(pi))),
                    dsl_parse_expr_stochastic(quote(a ~ Normal(0, sd)))))
})
