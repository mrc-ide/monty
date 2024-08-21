test_that("Return NULL where no domain given as input", {
  packer <- monty_packer(c("a", "b"))
  expect_null(monty_domain_expand(NULL, packer))
  expect_null(monty_domain_expand(matrix(numeric(), 0, 2), packer))
})


test_that("Error if given invalid domain matrix", {
  packer <- monty_packer(c("a", "b"))
  expect_error(
    monty_domain_expand(1:5, packer),
    "Expected 'domain' to be a matrix")
  expect_error(
    monty_domain_expand(cbind(1:5), packer),
    "Expected 'domain' to have 2 columns, but it had 1")
  expect_error(
    monty_domain_expand(cbind(1:5, 1:5), packer),
    "Expected 'domain' to have row names")
  expect_error(
    monty_domain_expand(rbind(a = 1:2, a = 1:2, b = 1:2), packer),
    "Duplicated entry in 'domain' rownames: 'a'")
  expect_error(
    monty_domain_expand(rbind(a = 1:2, a = 1:2, b = 1:2, b = 1:2), packer),
    "Duplicated entries in 'domain' rownames: 'a' and 'b'")
  expect_error(
    monty_domain_expand(rbind(a = 1:2, b = 1:2, x = 1:2), packer),
    "Unknown entry in 'domain' rownames: 'x'")
  expect_error(
    monty_domain_expand(rbind(a = 1:2, b = 1:2, x = 1:2, y = 1:2), packer),
    "Unknown entries in 'domain' rownames: 'x' and 'y'")
})


test_that("can expand parameters", {
  packer <- monty_packer(c("a", "b"), list(x = 3, y = c(2, 2)))
  expect_equal(
    monty_domain_expand(rbind(x = 0:1), packer),
    rbind("x[1]" = 0:1, "x[2]" = 0:1, "x[3]" = 0:1))
  expect_equal(
    monty_domain_expand(rbind(x = 0:1, "x[2]" = 2:3), packer),
    rbind("x[1]" = 0:1, "x[2]" = 2:3, "x[3]" = 0:1))
  expect_equal(
    monty_domain_expand(rbind(x = 0:1, y = 2:3), packer),
    rbind("x[1]" = 0:1, "x[2]" = 0:1, "x[3]" = 0:1,
          "y[1,1]" = 2:3, "y[2,1]" = 2:3 , "y[1,2]" = 2:3, "y[2,2]" = 2:3))
  expect_equal(
    monty_domain_expand(rbind(x = 0:1, b = 2:3, a = 4:5), packer),
    rbind(a = 4:5, b = 2:3, "x[1]" = 0:1, "x[2]" = 0:1, "x[3]" = 0:1))
})
