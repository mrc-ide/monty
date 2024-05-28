test_that("some transform corner cases", {
  expect_error(mcstate_transformer(),
               "Trying to generate an empty transformer")
})


test_that("trivial transformer", {
  xf <- mcstate_transformer("a")
  expect_equal(xf$parameters, "a")
  expect_equal(xf$transform(1), list(a = 1))
  expect_equal(xf$transform(c(a = 1)), list(a = 1))
  expect_error(xf$transform(c(b = 1)),
               "Incorrect names in input")
  expect_error(xf$transform(1:2),
               "Incorrect length input; expected 1 but given 2")
  expect_equal(xf$untransform(list(a = 1)), 1)
  expect_error(xf$untransform(list(a = 1:2)),
               "Invalid structure to untransform")
})


test_that("multiple scalar transformations", {
  xf <- mcstate_transformer(c("a", "b", "c"))
  expect_equal(xf$parameters, c("a", "b", "c"))
  expect_equal(xf$transform(1:3), list(a = 1, b = 2, c = 3))
  expect_equal(xf$untransform(list(a = 1, b = 2, c = 3)), 1:3)
})


test_that("can bind data into a transform", {
  xf <- mcstate_transformer(c("a", "b"), fixed = list(x = 1:5, y = 10))
  expect_equal(xf$parameters, c("a", "b"))
  expect_equal(xf$transform(1:2), list(a = 1, b = 2, x = 1:5, y = 10))
  expect_equal(xf$untransform(list(a = 1, b = 2, x = 1:5, y = 10)), 1:2)
  expect_equal(xf$untransform(list(a = 1, b = 2)), 1:2)
})


test_that("can transform arrays", {
  xf <- mcstate_transformer("a", list(b = 3))
  expect_equal(xf$parameters, c("a", "b[1]", "b[2]", "b[3]"))
  expect_equal(xf$transform(1:4), list(a = 1, b = 2:4))
})


test_that("can use integer vectors for array inputs", {
  xf <- mcstate_transformer("a", c(b = 3, c = 4))
  expect_equal(xf$parameters,
               c("a", sprintf("b[%d]", 1:3), sprintf("c[%d]", 1:4)))
  expect_equal(xf$transform(1:8), list(a = 1, b = 2:4, c = 5:8))
})
