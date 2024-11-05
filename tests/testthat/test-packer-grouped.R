test_that("can use a grouped packer", {
  p <- monty_packer_grouped(
    groups = c("x", "y", "z"),
    scalar = c("a", "b", "c", "d"),
    shared = c("b", "c"))
  expect_equal(
    p$groups(),
    c("x", "y", "z"))
  expect_equal(
    p$names(),
    c("b", "c", "a<x>", "d<x>", "a<y>", "d<y>", "a<z>", "d<z>"))
  expect_equal(
    p$unpack(1:8),
    list(x = list(a = 3, b = 1, c = 2, d = 4),
         y = list(a = 5, b = 1, c = 2, d = 6),
         z = list(a = 7, b = 1, c = 2, d = 8)))
  expect_equal(p$pack(p$unpack(1:8)), 1:8)
  expect_equal(p$index(), p$unpack(1:8))
  expect_error(p$subset(c("a", "d")),
               "subset() is not supported for grouped packers",
               fixed = TRUE)
})


test_that("can use a grouped packer with no shared parameters", {
  p <- monty_packer_grouped(
    groups = c("x", "y"),
    scalar = c("a", "b", "c", "d"))
  expect_equal(
    p$names(),
    c("a<x>", "b<x>", "c<x>", "d<x>", "a<y>", "b<y>", "c<y>", "d<y>"))
  expect_equal(p$unpack(1:8),
               list(x = list(a = 1, b = 2, c = 3, d = 4),
                    y = list(a = 5, b = 6, c = 7, d = 8)))
  expect_equal(p$pack(p$unpack(1:8)), 1:8)
})


test_that("can use a grouped packer with no varied parameters", {
  p <- monty_packer_grouped(
    groups = c("x", "y"),
    scalar = c("a", "b", "c", "d"),
    shared = c("a", "b", "c", "d"))
  expect_equal(
    p$names(),
    c("a", "b", "c", "d"))
  expect_equal(p$unpack(1:4),
               list(x = list(a = 1, b = 2, c = 3, d = 4),
                    y = list(a = 1, b = 2, c = 3, d = 4)))
  expect_equal(p$pack(p$unpack(1:4)), 1:4)
})


test_that("can use a grouped packer with arrays", {
  p <- monty_packer_grouped(
    groups = c("x", "y", "z"),
    scalar = c("a", "b"),
    array = list(c = 3, d = c(2, 2)),
    shared = "d")
  expect_length(p$names(), 19)
  expect_equal(
    p$unpack(1:19),
    list(x = list(a =  5, b =  6, c =  7:9,  d = matrix(1:4, 2, 2)),
         y = list(a = 10, b = 11, c = 12:14, d = matrix(1:4, 2, 2)),
         z = list(a = 15, b = 16, c = 17:19, d = matrix(1:4, 2, 2))))
  expect_equal(p$pack(p$unpack(1:19)), 1:19)
})


test_that("can use a grouped packer with fixed input", {
  p <- monty_packer_grouped(
    groups = c("x", "y"),
    scalar = letters[1:4],
    shared = "d",
    fixed = list(m = 1:4))
  expect_equal(
    p$unpack(1:7),
    list(x = list(a = 2, b = 3, c = 4, d = 1, m = 1:4),
         y = list(a = 5, b = 6, c = 7, d = 1, m = 1:4)))
  expect_equal(p$pack(p$unpack(1:7)), 1:7)
})


test_that("can use a grouped packer with shared and varied fixed input", {
  p <- monty_packer_grouped(
    groups = c("x", "y"),
    scalar = "a",
    fixed = list(b = 2, x = list(c = 3, d = 4), y = list(c = 5, d = 6)))
  expect_equal(
    p$unpack(c(0.1, 0.2)),
    list(x = list(a = 0.1, b = 2, c = 3, d = 4),
         y = list(a = 0.2, b = 2, c = 5, d = 6)))
  expect_equal(p$pack(p$unpack(c(0.1, 0.2))), c(0.1, 0.2))
})


test_that("varied fixed input can overwrite shared fixed input", {
  p <- monty_packer_grouped(
    groups = c("x", "y"),
    scalar = "a",
    fixed = list(b = 2, c = 3, x = list(d = 4), y = list(c = 5, d = 6)))
  expect_equal(
    p$unpack(c(0.1, 0.2)),
    list(x = list(a = 0.1, b = 2, c = 3, d = 4),
         y = list(a = 0.2, b = 2, c = 5, d = 6)))
  expect_equal(p$pack(p$unpack(c(0.1, 0.2))), c(0.1, 0.2))
})


test_that("shared must list distinct elements", {
  expect_error(
    monty_packer_grouped(
      groups = c("x", "y", "z"),
      scalar = c("a", "b", "c", "d"),
      shared = c("b", "b")),
    "Elements of 'shared' must be unique")
})


test_that("shared must list distinct members of the packer", {
  expect_error(
    monty_packer_grouped(
      groups = c("x", "y", "z"),
      scalar = c("a", "b", "c", "d"),
      shared = c("b", "e")),
    "Unknown value in 'shared' not present in 'scalar' or 'array': 'e'")
  expect_error(
    monty_packer_grouped(
      groups = c("x", "y", "z"),
      scalar = c("a", "b", "c", "d"),
      shared = c("b", "e", "f")),
    "Unknown values in 'shared' not present in 'scalar' or 'array': 'e'")
})


test_that("prevent varied names in fixed clashing with elements in packer", {
  expect_error(
    monty_packer_grouped(
      groups = c("x", "y", "z"),
      scalar = c("a", "b", "c", "d"),
      fixed = list(a = 5)),
    "Names must be distinct between 'scalar', 'array' and 'fixed'")
  expect_error(
    monty_packer_grouped(
      groups = c("x", "y", "z"),
      scalar = c("a", "b", "c", "d"),
      fixed = list(x = list(a = 5))),
    "Group-varying fixed element name clashes with 'scalar': 'a'")
  expect_error(
    monty_packer_grouped(
      groups = c("x", "y", "z"),
      scalar = c("a", "b", "c", "d"),
      fixed = list(x = list(x = 5))),
    "Group-varying fixed element name clashes with 'groups': 'x'")
})


test_that("grouped packers require at least two groups", {
  expect_error(
    monty_packer_grouped("x", c("a", "b")),
    "Expected at least two groups")
  expect_error(
    monty_packer_grouped(character(), c("a", "b")),
    "Expected at least two groups")
  expect_error(
    monty_packer_grouped(NULL, c("a", "b")),
    "Expected 'groups' to be character")
})


test_that("group names must not be used in names", {
  expect_error(
    monty_packer_grouped(c("x", "a"), c("a", "b")),
    "'groups' must be distinct from 'scalar' but 'a' was used in both")
  expect_error(
    monty_packer_grouped(c("x", "d"), c("a", "b"), list(c = 3, d = 4)),
    "'groups' must be distinct from 'array' but 'd' was used in both")
})


test_that("group varying fixed elements don't clash with other elements", {
  groups <- c("x", "y")
  scalar <- c("a", "b")
  array <- list(c = 2, d = 3)

  expect_error(
    monty_packer_grouped(groups, scalar, array, fixed = list(x = list(a = 1))),
    "Group-varying fixed element name clashes with 'scalar': 'a'")
  expect_error(
    monty_packer_grouped(groups, scalar, array, fixed = list(x = list(c = 1))),
    "Group-varying fixed element name clashes with 'array': 'c'")
  expect_error(
    monty_packer_grouped(groups, scalar, array, fixed = list(x = list(x = 1))),
    "Group-varying fixed element name clashes with 'groups': 'x'")
})


test_that("require that given array is correct size to unpack", {
  p <- monty_packer_grouped(c("x", "y"), c("a", "b"))
  expect_error(
    p$unpack(1),
    "Incorrect length input; expected 4 but given 1")
})


test_that("can't unpack matrix input yet", {
  p <- monty_packer_grouped(c("x", "y"), c("a", "b"))
  m <- matrix(1:8, 4, 2)
  expect_error(
    p$unpack(m),
    "Can't use unpack with matrix input and grouped packer yet")
})


test_that("when packing, validate that shared input is correct", {
  p <- monty_packer_grouped(c("x", "y"), c("a", "b"), shared = "a")
  expect_error(
    p$pack(list(x = c(a = 1, b = 2), y = c(a = 3, b = 4))),
    "Shared values are not identical across groups")
})


test_that("Can print a grouped packer", {
  p <- monty_packer_grouped(c("a", "b"), c("x", "y"))
  res <- evaluate_promise(withVisible(print(p)))
  expect_mapequal(res$result, list(value = p, visible = FALSE))
  expect_match(res$messages, "<monty_packer_grouped>",
               fixed = TRUE, all = FALSE)
  expect_match(res$messages,
               "Packing 4 values: 'x<a>', 'y<a>', 'x<b>', and 'y<b>",
               fixed = TRUE, all = FALSE)
})


test_that("can used process with grouped packer", {
  process <- function(res) {
    list(z = res$x + res$y)
  }
  p <- monty_packer_grouped(c("a", "b"), c("x", "y"), process = process)
  expect_equal(p$unpack(1:4),
               list(a = list(x = 1, y = 2, z = 3),
                    b = list(x = 3, y = 4, z = 7)))
})


test_that("can used process with grouped packer", {
  expect_error(
    monty_packer_grouped(c("a", "b"), c("x", "y"), process = TRUE),
    "Expected a function for 'process'")
})
