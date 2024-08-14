test_that("can't create empty packer", {
  expect_error(mcstate_packer(),
               "Trying to generate an empty packer")
})


test_that("trivial packer", {
  xp <- mcstate_packer("a")
  expect_equal(xp$parameters, "a")
  expect_equal(xp$unpack(1), list(a = 1))
  expect_equal(xp$unpack(c(a = 1)), list(a = 1))
  expect_error(xp$unpack(c(b = 1)),
               "Incorrect names in input")
  expect_error(xp$unpack(1:2),
               "Incorrect length input; expected 1 but given 2")
  expect_equal(xp$pack(list(a = 1)), 1)
  expect_error(xp$pack(list(a = 1:2)),
               "Invalid structure to 'pack()'",
               fixed = TRUE)
  expect_equal(xp$index(), list(a = 1))
})


test_that("multiple scalar unpacking", {
  xp <- mcstate_packer(c("a", "b", "c"))
  expect_equal(xp$parameters, c("a", "b", "c"))
  expect_equal(xp$unpack(1:3), list(a = 1, b = 2, c = 3))
  expect_equal(xp$pack(list(a = 1, b = 2, c = 3)), 1:3)
  expect_equal(xp$index(), list(a = 1, b = 2, c = 3))
})


test_that("can bind data into an unpacked list", {
  xp <- mcstate_packer(c("a", "b"), fixed = list(x = 1:5, y = 10))
  expect_equal(xp$parameters, c("a", "b"))
  expect_equal(xp$unpack(1:2), list(a = 1, b = 2, x = 1:5, y = 10))
  expect_equal(xp$pack(list(a = 1, b = 2, x = 1:5, y = 10)), 1:2)
  expect_equal(xp$pack(list(a = 1, b = 2)), 1:2)
})


test_that("can unpack arrays", {
  xp <- mcstate_packer("a", list(b = 3))
  expect_equal(xp$parameters, c("a", "b[1]", "b[2]", "b[3]"))
  expect_equal(xp$unpack(1:4), list(a = 1, b = 2:4))
})


test_that("can use integer vectors for array inputs", {
  xp <- mcstate_packer("a", c(b = 3, c = 4))
  expect_equal(xp$parameters,
               c("a", sprintf("b[%d]", 1:3), sprintf("c[%d]", 1:4)))
  expect_equal(xp$unpack(1:8), list(a = 1, b = 2:4, c = 5:8))
  expect_equal(xp$index(), list(a = 1, b = 2:4, c = 5:8))
})


test_that("can create packer with only arrays", {
  xp <- mcstate_packer(array = list(a = 2, b = 3))
  expect_equal(xp$unpack(1:5), list(a = 1:2, b = 3:5))
})


test_that("can create packers with higher-level dimensionsality", {
  xp <- mcstate_packer(
    array = list(a = 1, b = 2, c = 2:3, d = 2:4))
  expect_equal(
    xp$parameters,
    c("a[1]", "b[1]", "b[2]",
      sprintf("c[%d,%d]", 1:2, rep(1:3, each = 2)),
      sprintf("d[%d,%d,%d]", 1:2, rep(1:3, each = 2), rep(1:4, each = 6))))
  expect_equal(
    xp$unpack(1:33),
    list(a = 1, b = 2:3, c = matrix(4:9, 2, 3), d = array(10:33, 2:4)))
  expect_equal(
    xp$pack(xp$unpack(1:33)),
    1:33)
})


test_that("names for scalar, array and fixed must be reasonable", {
  expect_error(
    mcstate_packer(c("a", "b", "a")),
    "Elements of 'scalar' must be unique")
  expect_error(
    mcstate_packer("a", list(1)),
    "'array' must be named")
  expect_error(
    mcstate_packer("a", list(b = 1, c = 2, b = 3)),
    "'array' must have unique names")
  expect_error(
    mcstate_packer("a", fixed = list(x = 1, y = 2, x = 1)),
    "'fixed' must have unique names")
  expect_error(
    mcstate_packer(c("a", "b", "c"), list(b = 2, d = 3)),
    "Names must be distinct between 'scalar', 'array' and 'fixed'")
  expect_error(
    mcstate_packer(c("a", "b", "c"), list(d = 2, e = 3), list(d = 5)),
    "Names must be distinct between 'scalar', 'array' and 'fixed'")
})


test_that("validate array inputs", {
  expect_error(
    mcstate_packer(array = list(a = integer(), b = 2)),
    "Elements of 'array' must have at least one element, but 'a' has none")
  expect_error(
    mcstate_packer(array = list(a = 1, b = 2.5)),
    "Elements of 'array' must be integer-like vectors, but 'b' is not")
  expect_error(
    mcstate_packer(array = list(a = 1, b = c(3, 0, 2))),
    "All dimensions in 'array' must be at least 1, but 'b' violates this")
})


test_that("require that scalar is a character vector", {
  expect_error(
    mcstate_packer(1:2),
    "Expected a character vector for 'scalar'")
})


test_that("can post-process parameters", {
  p <- function(x) {
    list(d = x$a + x$b + x$c)
  }
  xp <- mcstate_packer(c("a", "b", "c"), process = p)
  expect_equal(xp$parameters, c("a", "b", "c"))
  expect_equal(xp$unpack(1:3), list(a = 1, b = 2, c = 3, d = 6))
  expect_equal(xp$pack(xp$unpack(1:3)), 1:3)
  expect_equal(xp$pack(list(a = 1, b = 2, c = 3)), 1:3)
})


test_that("reject non-function process arguments", {
  expect_error(
    mcstate_packer(c("a", "b", "c"), process = TRUE),
    "Expected a function for 'process'")
})


test_that("require that process is well-behaved", {
  p <- function(x) {
    c(x, list(d = x$a + x$b + x$c))
  }
  xp <- mcstate_packer(c("a", "b", "c"), process = p)
  expect_error(xp$unpack(1:3),
               "'process()' is trying to overwrite entries in parameters",
               fixed = TRUE)
})


test_that("Can print a packer", {
  p <- mcstate_packer(c("x", "y"))
  res <- evaluate_promise(withVisible(print(p)))
  expect_mapequal(res$result, list(value = p, visible = FALSE))
  expect_match(res$messages, "<mcstate_packer>",
               fixed = TRUE, all = FALSE)
  expect_match(res$messages, "Packing 2 parameters: 'x' and 'y'",
               fixed = TRUE, all = FALSE)
})
