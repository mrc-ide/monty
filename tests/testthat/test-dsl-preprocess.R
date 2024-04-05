test_that("can accept dsl input from expressions", {
  expect_equal(
    dsl_preprocess(quote(a ~ Normal(0, 1))),
    list(quote(a ~ Normal(0, 1))))
  expect_equal(
    dsl_preprocess(quote({
      a ~ Normal(0, 1)
    })),
    list(quote(a ~ Normal(0, 1))))
  expect_equal(
    dsl_preprocess(quote({
      a ~ Normal(0, 1)
      b ~ Normal(a, 1)
    })),
    list(quote(a ~ Normal(0, 1)),
         quote(b ~ Normal(a, 1))))
})

test_that("can accept dsl input from text, preserving source", {
  expect_equal(
    dsl_preprocess("a ~ Normal(0, 1)"),
    list(structure(quote(a ~ Normal(0, 1)),
                   line = 1, str = "a ~ Normal(0, 1)")))
  expect_equal(
    dsl_preprocess("a ~ Normal(0, 1)\nb~Normal(a,1)"),
    list(structure(quote(a ~ Normal(0, 1)),
                   line = 1, str = "a ~ Normal(0, 1)"),
         structure(quote(b ~ Normal(a, 1)),
                   line = 2, str = "b~Normal(a,1)")))
  expect_equal(
    dsl_preprocess("a ~ Normal(0, 1)\n# comment\nb~Normal(a,1)"),
    list(structure(quote(a ~ Normal(0, 1)),
                   line = 1, str = "a ~ Normal(0, 1)"),
         structure(quote(b ~ Normal(a, 1)),
                   line = 3, str = "b~Normal(a,1)")))
  expect_equal(
    dsl_preprocess("a ~ Normal(0, 1)\n# comment\nb~Normal(\na,\n1)"),
    list(structure(quote(a ~ Normal(0, 1)),
                   line = 1, str = "a ~ Normal(0, 1)"),
         structure(quote(b ~ Normal(a, 1)),
                   line = 3, str = c("b~Normal(", "a,", "1)"))))

  expect_equal(
    dsl_preprocess("a ~ Normal(0, 1)\n# comment\nb~Normal(\n# x\na,\n1)"),
    list(structure(quote(a ~ Normal(0, 1)),
                   line = 1, str = "a ~ Normal(0, 1)"),
         structure(quote(b ~ Normal(a, 1)),
                   line = 3, str = c("b~Normal(", "# x", "a,", "1)"))))
})


test_that("can accept dsl input from file, preserving source", {
  tmp <- withr::local_tempfile()
  writeLines("a ~ Normal(0, 1)\n# comment\nb~Normal(\na,\n1)", tmp)
  expect_equal(
    dsl_preprocess(tmp),
    list(structure(quote(a ~ Normal(0, 1)),
                   line = 1, str = "a ~ Normal(0, 1)"),
         structure(quote(b ~ Normal(a, 1)),
                   line = 3, str = c("b~Normal(", "a,", "1)"))))
})


test_that("detect text inputs", {
  expect_equal(preprocess_detect("a <- 1", NULL), "text")
  expect_equal(preprocess_detect("a ~ 1", NULL), "text")
  expect_equal(preprocess_detect("a\n1", NULL), "text")
  expect_equal(preprocess_detect("a;1", NULL), "text")
  expect_equal(preprocess_detect(c("a", "b"), NULL), "text")
})


test_that("require input is text, when type given", {
  expect_equal(preprocess_detect("a", "text"), "text")
  expect_error(preprocess_detect(quote(a), "text"),
               "Invalid input for 'x': expected text")
})


test_that("detect filename inputs", {
  expect_error(
    preprocess_detect("a", NULL),
    "'a' looks like a filename, but file does not exist")
  tmp <- withr::local_tempfile()
  file.create(tmp)
  expect_equal(preprocess_detect(tmp, NULL), "file")
})


test_that("require input is file, when type given", {
  tmp <- withr::local_tempfile()
  file.create(tmp)
  expect_equal(preprocess_detect(tmp, "file"), "file")
  expect_error(preprocess_detect("a", "file"),
               "File 'a' does not exist")
})


test_that("detect expression inputs", {
  expect_equal(preprocess_detect(quote(a), NULL), "expression")
  expect_equal(preprocess_detect(quote({a}), NULL), "expression")
  expect_equal(preprocess_detect(quote({a}), "expression"), "expression")
})


test_that("require input is expressions, when type given", {
  expect_equal(preprocess_detect(quote({a}), "expression"), "expression")
  expect_error(preprocess_detect("a", "expression"),
               "Invalid input for 'x': expected expression")
})


test_that("error for inputs we can't use", {
  expect_error(preprocess_detect(1, NULL),
               "Invalid input for 'x'")
})


test_that("avoid extra quoting", {
  expect_error(
    dsl_preprocess(quote(quote(a ~ Normal(0, 1)))),
    "You have an extra layer of quote() around 'x'")
})
