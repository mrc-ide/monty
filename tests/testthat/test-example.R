test_that("error if unexpected example used", {
  expect_error(
    monty_example("unknown"),
    "'name' must be one of")
})
