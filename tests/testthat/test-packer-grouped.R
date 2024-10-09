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
