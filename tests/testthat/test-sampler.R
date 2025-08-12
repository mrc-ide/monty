test_that("construct empty sampler state", {
  state <- monty_sampler2_state(NULL, NULL, NULL, NULL)
  expect_null(state$dump())
  expect_null(state$restore())
  expect_null(state$combine())
  expect_null(state$details())
})


test_that("error if dump provided but other functions are not", {
  expect_error(
    monty_sampler2_state(identity, NULL, NULL, NULL),
    "Missing state handling functions: 'state_restore' and 'state_combine'")
  expect_error(
    monty_sampler2_state(identity, NULL, identity, NULL),
    "Missing state handling function: 'state_combine'")
})


test_that("Allow restore even where dump is missing", {
  state <- monty_sampler2_state(NULL, NULL, identity, NULL)
  expect_identical(state$restore, identity)
})


test_that("Don't allow other functions if dump is missing", {
  expect_error(
    monty_sampler2_state(NULL, identity, NULL, NULL),
    "Unexpected state handling function provided: 'state_combine'")
  expect_error(
    monty_sampler2_state(NULL, identity, NULL, identity),
    "Unexpected state handling functions provided: 'state_combine'")
})
