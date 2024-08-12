test_that("can print a runner object", {
  r <- mcstate_runner_serial()
  res <- evaluate_promise(withVisible(print(r)))
  expect_mapequal(res$result, list(value = r, visible = FALSE))
  expect_match(
    res$messages,
    "<mcstate_runner: Serial (mcstate_runner_serial)>",
    fixed = TRUE, all = FALSE)
})
