test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("reject inappropriate vcv matrices", {
  expect_error(check_vcv(1, "m"),
               "Expected 'm' to be a matrix")
  expect_error(check_vcv(matrix(runif(6), 2, 3), "m"),
               "Expected 'm' to be symmetric")
  expect_error(check_vcv(matrix(runif(16), 4, 4), "m"),
               "Expected 'm' to be symmetric")
  m <- var(mtcars[1:3])
  expect_no_error(check_vcv(m))
  diag(m) <- 0
  expect_error(check_vcv(m), "Expected 'm' to be positive definite")
})
