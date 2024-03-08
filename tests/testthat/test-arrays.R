test_that("reshape", {
  m4 <- random_array(c(5, 7, 10, 13))
  res <- array_reshape(m4, 3L, c(2, 5))
  expect_equal(dim(res), c(5, 7, 2, 5, 13))
  expect_equal(c(res), c(m4))

  expect_equal(res[2, 2, , ,  2], matrix(m4[2, 2, , 2], 2, 5))

  expect_error(
    array_reshape(m4, 10, c(2, 5)),
    "array only has 4 dimensions, can't update dimension 10")
  expect_error(
    array_reshape(m4, 3, c(2, 6)),
    "New dimensions (2, 6) imply dimension 3 has length 12 but found 10",
    fixed = TRUE)
})


test_that("reshape preserves dimnames where it can", {
  m4 <- random_array(c(5, 7, 10, 13), TRUE)
  res <- array_reshape(m4, 3L, c(2, 5))
  expect_equal(dim(res), c(5, 7, 2, 5, 13))
  expect_equal(dimnames(res),
               c(dimnames(m4)[1:2],
                 list(NULL, NULL),
                 dimnames(m4)[4]))

  dimnames(m4) <- NULL
  rownames(m4) <- letters[1:5]
  res <- array_reshape(m4, 3L, c(2, 5))
  expect_equal(dim(res), c(5, 7, 2, 5, 13))
  expect_equal(dimnames(res),
               list(letters[1:5], NULL, NULL, NULL, NULL))
})


test_that("array_bind", {
  m2 <- random_array(c(5, 10))

  expect_identical(array_bind(m2[, 1:4], m2[, 5:10]), m2)

  m3 <- random_array(c(5, 7, 10))
  expect_identical(array_bind(m3[, , 1:4], m3[, , 5:10]), m3)

  m4 <- random_array(c(5, 7, 3, 10))
  expect_identical(array_bind(m4[, , , 1:4], m4[, , , 5:10]), m4)
})


test_that("array_bind on other dimensions", {
  m2 <- random_array(c(10, 5))
  expect_identical(array_bind(m2[1:4, ], m2[5:10, ], on = 1), m2)

  m3 <- random_array(c(5, 7, 9))
  expect_identical(array_bind(m3[1:2, , ], m3[3:5, , ], on = 1), m3)
  expect_identical(array_bind(m3[, 1:3, ], m3[, 4:7, ], on = 2), m3)
  expect_identical(array_bind(m3[, , 1:4], m3[, , 5:9], on = 3), m3)

  m4 <- random_array(c(5, 7, 3, 10))
  expect_identical(array_bind(m4[1:2, , , ], m4[3:5, , , ], on = 1), m4)
})


test_that("preserve dimension names on merge", {
  drop_last_names <- function(m) {
    dn <- dimnames(m)
    dn[length(dn)] <- list(NULL)
    dimnames(m) <- dn
    m
  }

  m2 <- random_array(c(5, 10), TRUE)
  expect_identical(array_bind(m2[, 1:4], m2[, 5:10]), m2)
  expect_identical(array_bind(drop_last_names(m2[, 1:4]), m2[, 5:10]),
                   drop_last_names(m2))
  expect_identical(array_bind(m2[, 1:4], drop_last_names(m2[, 5:10])),
                   drop_last_names(m2))

  m3 <- random_array(c(5, 7, 10), TRUE)
  expect_identical(array_bind(m3[, , 1:4], m3[, , 5:10]), m3)

  m4 <- random_array(c(5, 7, 3, 10), TRUE)
  expect_identical(array_bind(m4[, , , 1:4], m4[, , , 5:10]), m4)
})


test_that("Can't merge incompatible arrays", {
  m4 <- random_array(c(5, 7, 3, 10))
  expect_error(
    array_bind(m4[, , , 1:4], m4[, , -1, 5:10]),
    "array 2 (dimension 3)",
    fixed = TRUE)
  expect_error(
    array_bind(m4[, , , 1:4], m4[-1, , , 1:4], m4[-1, -1, -1, 5:10]),
    "array 2 \\(dimension 1\\).*array 3 \\(dimension 1, 2, 3\\)")
  expect_error(
    array_bind(m4[1:3, , , ], m4[1:3, , , -1], on = 1),
    "array 2 (dimension 4)",
    fixed = TRUE)
  expect_error(
    array_bind(m4[, , , 1:4], random_array(c(5, 7, 10))),
    "Can't bind these arrays together, they do not have the same rank",
    fixed = TRUE)
})


test_that("can bind together by adding a dimension", {
  m1 <- random_array(c(3, 5))
  m2 <- random_array(c(3, 5))

  res1 <- array_bind(arrays = list(m1, m2), after = 1)
  expect_equal(res1[, 1, ], m1)
  expect_equal(res1[, 2, ], m2)
  expect_equal(
    array_bind(arrays = list(m1, m2), before = 2),
    res1)

  res2 <- array_bind(arrays = list(m1, m2), after = 2)
  expect_equal(res2[, , 1], m1)
  expect_equal(res2[, , 2], m2)

  res3 <- array_bind(arrays = list(m1, m2), before = 1)
  expect_equal(res3[1, , ], m1)
  expect_equal(res3[2, , ], m2)
})


test_that("trivial case", {
  m4 <- random_array(c(5, 7, 3, 10))
  expect_identical(array_bind(m4), m4)
  expect_error(array_bind(), "Must provide at least one array")
})


test_that("prevent conflicting arguments", {
  m1 <- random_array(c(3, 5))
  m2 <- random_array(c(3, 5))
  expect_error(
    array_bind(arrays = list(m1, m2), after = 1, before = 2),
    "Only one of 'on', 'before' or 'after' may be given")
  expect_error(
    array_bind(arrays = list(m1, m2), after = 1, on = 2),
    "Only one of 'on', 'before' or 'after' may be given")
})


test_that("binding after requires that the provided values are reasonable", {
  m1 <- random_array(c(3, 5))
  m2 <- random_array(c(3, 5))

  expect_error(
    array_bind(arrays = list(m1, m2), before = 3),
    "Invalid value for 'before' (3), must be in [1, 2]",
    fixed = TRUE)
  expect_error(
    array_bind(arrays = list(m1, m2), before = 0),
    "Invalid value for 'before' (0), must be in [1, 2]",
    fixed = TRUE)
  expect_error(
    array_bind(arrays = list(m1, m2), after = 3),
    "Invalid value for 'after' (3), must be in [1, 2]",
    fixed = TRUE)
  expect_error(
    array_bind(arrays = list(m1, m2), after = 0),
    "Invalid value for 'after' (0), must be in [1, 2]",
    fixed = TRUE)
})


test_that("Can get an arbitrary dimension of an array", {
  m1 <- random_array(4)
  expect_equal(array_nth_dimension(m1, 1, 2), m1[2])
  expect_equal(array_nth_dimension(m1, 1, 2:3), m1[2:3])

  m2 <- random_array(c(3, 5))
  expect_equal(array_nth_dimension(m2, 2, 2), m2[, 2, drop = FALSE])
  expect_equal(array_nth_dimension(m2, 2, 2:3), m2[, 2:3, drop = FALSE])

  m3 <- random_array(c(3, 5, 7))
  expect_equal(array_nth_dimension(m3, 2, 2), m3[, 2, , drop = FALSE])
  expect_equal(array_nth_dimension(m3, 2, 2:3), m3[, 2:3, , drop = FALSE])

  m4 <- random_array(c(3, 5, 7, 11))
  expect_equal(array_nth_dimension(m4, 2, 2), m4[, 2, , , drop = FALSE])
  expect_equal(array_nth_dimension(m4, 2, 2:3), m4[, 2:3, , , drop = FALSE])

  m5 <- random_array(c(3, 5, 7, 5, 3))
  expect_equal(array_nth_dimension(m5, 2, 2), m5[, 2, , , , drop = FALSE])
  expect_equal(array_nth_dimension(m5, 2, 2:3), m5[, 2:3, , , , drop = FALSE])

  m6 <- random_array(c(3, 5, 7, 5, 3, 5))
  expect_equal(array_nth_dimension(m6, 2, 2), m6[, 2, , , , , drop = FALSE])
  expect_equal(array_nth_dimension(m6, 2, 2:3), m6[, 2:3, , , , , drop = FALSE])

  m7 <- random_array(c(3, 5, 7, 5, 3, 5, 7))
  expect_equal(array_nth_dimension(m7, 2, 2),
               m7[, 2, , , , , , drop = FALSE])
  expect_equal(array_nth_dimension(m7, 2, 2:3),
               m7[, 2:3, , , , , , drop = FALSE])

  expect_error(
    array_nth_dimension(array(0, rep(1, 8)), 2, 2),
    "Unexpected rank")
  expect_error(
    array_nth_dimension(m2, 3, 2),
    "'k' must be in [1, 2]",
    fixed = TRUE)
})


test_that("drop spare dimensions", {
  m4 <- random_array(c(5, 1, 10, 1))
  res1 <- array_drop(m4, 2)
  expect_equal(c(m4), c(res1))
  expect_equal(dim(res1), c(5, 10, 1))

  res2 <- array_drop(m4, c(2, 4))
  expect_equal(c(m4), c(res2))
  expect_equal(dim(res2), c(5, 10))
})


test_that("preserve names when dropping dimensions", {
  m4 <- random_array(c(5, 1, 10, 1), TRUE)
  expect_equal(dimnames(array_drop(m4, 2)), dimnames(m4)[-2])
  expect_equal(dimnames(array_drop(m4, c(2, 4))), dimnames(m4)[-c(2, 4)])
})


test_that("Prevent impossible drops", {
  m4 <- random_array(c(5, 1, 10, 1))
  expect_error(
    array_drop(m4, c(2, 5)),
    "Can't update dimension 5, array only has 4 dimensions")

  expect_error(array_drop(m4, 1),
               "Can't drop dimension 1 as it is length 5, not 1")
  expect_error(
    array_drop(m4, c(1, 3)),
    "Can't drop dimensions 1 and 3 as they are length 5 and 10, not 1",
    fixed = TRUE)
  expect_error(
    array_drop(m4, c(1, 2, 3)),
    "Can't drop dimensions 1 and 3 as they are length 5 and 10, not 1",
    fixed = TRUE)
})
