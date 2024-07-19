test_that("throw useful error if we have new unimplemented expression types", {
  expect_error(
    dsl_generate_density_expr(list(type = "magic")),
    "Unimplemented expression type 'magic'; this is an mcstate2 bug")
  expect_error(
    dsl_generate_sample_expr(list(type = "magic")),
    "Unimplemented expression type 'magic'; this is an mcstate2 bug")
})
