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
