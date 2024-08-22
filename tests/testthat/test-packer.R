test_that("can't create empty packer", {
  expect_error(monty_packer(),
               "Trying to generate an empty packer")
})


test_that("trivial packer", {
  xp <- monty_packer("a")
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
  xp <- monty_packer(c("a", "b", "c"))
  expect_equal(xp$parameters, c("a", "b", "c"))
  expect_equal(xp$unpack(1:3), list(a = 1, b = 2, c = 3))
  expect_equal(xp$pack(list(a = 1, b = 2, c = 3)), 1:3)
  expect_equal(xp$index(), list(a = 1, b = 2, c = 3))
})


test_that("can bind data into an unpacked list", {
  xp <- monty_packer(c("a", "b"), fixed = list(x = 1:5, y = 10))
  expect_equal(xp$parameters, c("a", "b"))
  expect_equal(xp$unpack(1:2), list(a = 1, b = 2, x = 1:5, y = 10))
  expect_equal(xp$pack(list(a = 1, b = 2, x = 1:5, y = 10)), 1:2)
  expect_equal(xp$pack(list(a = 1, b = 2)), 1:2)
})


test_that("can unpack arrays", {
  xp <- monty_packer("a", list(b = 3))
  expect_equal(xp$parameters, c("a", "b[1]", "b[2]", "b[3]"))
  expect_equal(xp$unpack(1:4), list(a = 1, b = 2:4))
})


test_that("can use integer vectors for array inputs", {
  xp <- monty_packer("a", c(b = 3, c = 4))
  expect_equal(xp$parameters,
               c("a", sprintf("b[%d]", 1:3), sprintf("c[%d]", 1:4)))
  expect_equal(xp$unpack(1:8), list(a = 1, b = 2:4, c = 5:8))
  expect_equal(xp$index(), list(a = 1, b = 2:4, c = 5:8))
})


test_that("can create packer with only arrays", {
  xp <- monty_packer(array = list(a = 2, b = 3))
  expect_equal(xp$unpack(1:5), list(a = 1:2, b = 3:5))
})


test_that("can create packers with higher-level dimensionsality", {
  xp <- monty_packer(
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
    monty_packer(c("a", "b", "a")),
    "Elements of 'scalar' must be unique")
  expect_error(
    monty_packer("a", list(1)),
    "'array' must be named")
  expect_error(
    monty_packer("a", list(b = 1, c = 2, b = 3)),
    "'array' must have unique names")
  expect_error(
    monty_packer("a", fixed = list(x = 1, y = 2, x = 1)),
    "'fixed' must have unique names")
  expect_error(
    monty_packer(c("a", "b", "c"), list(b = 2, d = 3)),
    "Names must be distinct between 'scalar', 'array' and 'fixed'")
  expect_error(
    monty_packer(c("a", "b", "c"), list(d = 2, e = 3), list(d = 5)),
    "Names must be distinct between 'scalar', 'array' and 'fixed'")
})


test_that("validate array inputs", {
  expect_error(
    monty_packer(array = list(a = 1, b = 2.5)),
    "Elements of 'array' must be integer-like vectors, but 'b' is not")
  expect_error(
    monty_packer(array = list(a = 1, b = c(3, 0, 2))),
    "All dimensions in 'array' must be at least 1, but 'b' violates this")
})


test_that("can pass empty array elements as scalars", {
  p <- monty_packer(array = list(a = integer(), b = 2))
  expect_equal(p$parameters, c("a", "b[1]", "b[2]"))
  expect_equal(p$unpack(1:3), list(a = 1, b = 2:3))
  expect_equal(p$pack(list(a = 1, b = 2:3)), 1:3)
})


test_that("can pass empty array elements as scalars in odd order", {
  p <- monty_packer(array = list(a = 2, b = NULL))
  expect_equal(p$parameters, c("a[1]", "a[2]", "b"))
  expect_equal(p$unpack(1:3), list(a = 1:2, b = 3))
  expect_equal(p$pack(list(a = 1:2, b = 3)), 1:3)
})


test_that("require that scalar is a character vector", {
  expect_error(
    monty_packer(1:2),
    "Expected a character vector for 'scalar'")
})


test_that("can post-process parameters", {
  p <- function(x) {
    list(d = x$a + x$b + x$c)
  }
  xp <- monty_packer(c("a", "b", "c"), process = p)
  expect_equal(xp$parameters, c("a", "b", "c"))
  expect_equal(xp$unpack(1:3), list(a = 1, b = 2, c = 3, d = 6))
  expect_equal(xp$pack(xp$unpack(1:3)), 1:3)
  expect_equal(xp$pack(list(a = 1, b = 2, c = 3)), 1:3)
})


test_that("reject non-function process arguments", {
  expect_error(
    monty_packer(c("a", "b", "c"), process = TRUE),
    "Expected a function for 'process'")
})


test_that("require that process is well-behaved", {
  p <- function(x) {
    c(x, list(d = x$a + x$b + x$c))
  }
  xp <- monty_packer(c("a", "b", "c"), process = p)
  expect_error(xp$unpack(1:3),
               "'process()' is trying to overwrite entries in parameters",
               fixed = TRUE)
})


test_that("Can print a packer", {
  p <- monty_packer(c("x", "y"))
  res <- evaluate_promise(withVisible(print(p)))
  expect_mapequal(res$result, list(value = p, visible = FALSE))
  expect_match(res$messages, "<monty_packer>",
               fixed = TRUE, all = FALSE)
  expect_match(res$messages, "Packing 2 parameters: 'x' and 'y'",
               fixed = TRUE, all = FALSE)
})


test_that("unpack a matrix", {
  p <- monty_packer("x", list(y = 5, z = c(2, 3))) # > 1 + 5 + 6 = 12
  m <- matrix(seq_len(12 * 3), 12)

  res <- p$unpack(m)
  expect_equal(res$x, c(1, 13, 25))
  expect_equal(res$y, cbind(2:6, 14:18, 26:30))
  expect_equal(res$z, array(c(7:12, 19:24, 31:36), c(2, 3, 3)))
})


test_that("error if given the wrong size input to unpack", {
  p <- monty_packer("x", list(y = 5, z = c(2, 3))) # > 1 + 5 + 6 = 12
  m <- matrix(seq_len(12 * 3), 9)
  expect_error(
    p$unpack(m),
    "Incorrect length of first dimension of input; expected 12 but given 9")
})


test_that("error if given the wrong size input to unpack", {
  p <- monty_packer("x", list(y = 5, z = c(2, 3))) # > 1 + 5 + 6 = 12
  m <- matrix(seq_len(12 * 3), 12)
  rownames(m) <- letters[1:12]
  expect_error(
    p$unpack(m),
    "Incorrect rownames in input")
})


test_that("can't used fixed with array unpacking", {
  p <- monty_packer(c("x", "y"), fixed = list(a = 10))
  expect_equal(p$unpack(1:2), list(x = 1, y = 2, a = 10))
  expect_error(
    p$unpack(matrix(1:6, 2)),
    "Can't unpack a matrix where the unpacker uses 'fixed'")
})


test_that("can't used process with array unpacking", {
  p <- monty_packer(c("x", "y"), process = function(d) list(z = d$x + d$y))
  expect_equal(p$unpack(1:2), list(x = 1, y = 2, z = 3))
  expect_error(
    p$unpack(matrix(1:6, 2)),
    "Can't unpack a matrix where the unpacker uses 'process'")
})
