test_that("can validate sampler properties", {
  properties <- validate_sampler_properties(NULL, NULL, NULL)
  expect_s3_class(properties, "monty_sampler_properties")
  expect_false(properties$has_state)
  expect_false(properties$restartable)
})


test_that("can validate sampler properties", {
  properties <- monty_sampler_properties(has_state = TRUE, restartable = TRUE)
  expect_error(
    validate_sampler_properties(properties, NULL, NULL),
    "A 'state_dump' function is required because sampler properties include")
  expect_error(
    validate_sampler_properties(properties, identity, NULL),
    "A 'state_restore' function is required because sampler properties include")
})


test_that("can validate sampler properties", {
  properties <- validate_sampler_properties(NULL, NULL, NULL)
  expect_s3_class(properties, "monty_sampler_properties")
  expect_false(properties$has_state)
  expect_false(properties$restartable)
})


test_that("construct empty sampler state", {
  state <- monty_sampler_state(NULL, NULL, NULL, NULL)
  expect_null(state$dump())
  expect_null(state$restore())
  expect_null(state$combine())
  expect_null(state$details())
})


test_that("error if dump provided but other functions are not", {
  expect_error(
    monty_sampler_state(identity, NULL, NULL, NULL),
    "Missing state handling functions: 'state_restore' and 'state_combine'")
  expect_error(
    monty_sampler_state(identity, NULL, identity, NULL),
    "Missing state handling function: 'state_combine'")
})


test_that("Allow restore even where dump is missing", {
  state <- monty_sampler_state(NULL, NULL, identity, NULL)
  expect_identical(state$restore, identity)
})


test_that("Don't allow other functions if dump is missing", {
  expect_error(
    monty_sampler_state(NULL, identity, NULL, NULL),
    "Unexpected state handling function provided: 'state_combine'")
  expect_error(
    monty_sampler_state(NULL, identity, NULL, identity),
    "Unexpected state handling functions provided: 'state_combine'")
})


test_that("can create a custom sampler", {
  ## This also acts as a test that we can construct a stateless
  ## sampler.
  toy_sampler <- function(sd) {
    control <- list(sd = sd)
    monty_sampler(
      "Toy Sampler",
      "toy_sampler",
      control,
      toy_sampler_initialise,
      toy_sampler_step)
  }

  sampler <- toy_sampler(rep(0.2, 5))
  model <- monty_example("gaussian", diag(5))
  samples1 <- monty_sample(model, sampler, 100, restartable = TRUE)
  samples2 <- monty_sample_continue(samples1, 50)
  expect_s3_class(samples2, "monty_samples")
})
