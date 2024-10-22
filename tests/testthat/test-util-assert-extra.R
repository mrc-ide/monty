test_that("can used assert_named_with", {
  x <- list(a = 1, b = 2, c = 3)
  nms <- names(x)
  expect_no_error(assert_named_with(x, nms))
  expect_no_error(assert_named_with(unname(x), nms, required = FALSE))
  expect_error(
    assert_named_with(unname(x), nms, required = TRUE),
    "Unexpected names for")
  expect_error(
    assert_named_with(x, toupper(nms), required = TRUE),
    "Unexpected names for")
})
