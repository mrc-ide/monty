test_that("can join dependencies", {
  deps <- list(
    list(functions = c("a", "f", "g"), variables = c("a", "b", "c")),
    list(functions = "f",              variables = c("c", "d", "e")),
    list(functions = c("h", "i"),      variables = character()))
  expect_equal(
    join_dependencies(list()),
    list(functions = character(), variables = character()))
  expect_equal(
    join_dependencies(deps[1]),
    deps[[1]])
  expect_equal(
    join_dependencies(deps),
    list(functions = c("a", "f", "g", "h", "i"),
         variables = c("a", "b", "c", "d", "e")))
})
