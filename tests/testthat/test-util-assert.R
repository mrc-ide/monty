test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})


test_that("assert_character", {
  expect_silent(assert_character("a"))
  expect_error(assert_character(1), "to be character")
  expect_error(assert_character(TRUE), "to be character")
})


test_that("assert_logical", {
  expect_silent(assert_logical(TRUE))
  expect_error(assert_logical(1), "Expected '1' to be logical")
  expect_error(assert_logical("a"), "Expected '\"a\"' to be logical")
})


test_that("assert_nonmissing", {
  expect_silent(assert_nonmissing(TRUE))
  expect_error(assert_nonmissing(NA), "Expected 'NA' to be non-NA")
  x <- c(1, NA)
  expect_error(assert_nonmissing(x), "Expected 'x' to be non-NA")
})


test_that("match_value", {
  expect_error(match_value("foo", letters), "must be one of")
  expect_silent(match_value("a", letters))
})


test_that("assert_list", {
  expect_silent(assert_list(list()))
  expect_silent(assert_list(list(a = 1)))
  x <- c(a = 1)
  expect_error(assert_list(x), "Expected 'x' to be a list")
})
