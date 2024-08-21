test_that("can print a runner object", {
  r <- monty_runner_serial()
  res <- evaluate_promise(withVisible(print(r)))
  expect_mapequal(res$result, list(value = r, visible = FALSE))
  expect_match(
    res$messages,
    "<monty_runner: Serial (monty_runner_serial)>",
    fixed = TRUE, all = FALSE)
})
