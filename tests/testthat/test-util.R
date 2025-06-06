test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("reject inappropriate vcv matrices", {
  expect_error(check_vcv(1, name = "m"),
               "Expected a matrix for 'm'")
  expect_error(check_vcv(matrix(runif(6), 2, 3), name = "m"),
               "Expected 'm' to be symmetric")
  expect_error(check_vcv(matrix(runif(16), 4, 4), name = "m"),
               "Expected 'm' to be symmetric")
  m <- var(mtcars[1:3])
  expect_no_error(check_vcv(m))
  diag(m) <- 0
  expect_error(check_vcv(m), "Expected 'm' to be positive definite")
})


test_that("reject inappropriate vcv arrays", {
  vcv <- array(0, c(2, 2, 5))
  vcv[1, 1, ] <- 1:5
  vcv[2, 2, ] <- 1
  expect_no_error(check_vcv(vcv, allow_3d = TRUE, name = "m"))
  expect_error(
    check_vcv(vcv, allow_3d = FALSE, name = "m"),
    "Expected a matrix for 'm'")
  expect_error(
    check_vcv(array(vcv, c(2, 2, 5, 1)), allow_3d = TRUE, name = "m"),
    "Expected a matrix or 3d array for 'm'")
  expect_error(
    check_vcv(vcv[, , 0], allow_3d = TRUE, name = "m"),
    "At least one vcv required within a vcv array")
  vcv[1, 2, 2] <- 10
  expect_error(
    check_vcv(vcv, allow_3d = TRUE, name = "m"),
    "Expected 'm[, , 2]' to be symmetric",
    fixed = TRUE)
  vcv[2, 1, 2] <- 10
  expect_error(
    check_vcv(vcv, allow_3d = TRUE, name = "m"),
    "Expected 'm[, , 2]' to be positive definite",
    fixed = TRUE)
})


test_that("can bind lists of matrices", {
  m1 <- matrix(1:6, 3, 2)
  m2 <- matrix(7:12, 3, 2)
  m3 <- matrix(13:16, 2, 2)

  expect_equal(rbind_list(list(m1)), m1)
  expect_equal(rbind_list(list(m1, m2)), rbind(m1, m2))
  expect_equal(rbind_list(list(m1, m2, m3)), rbind(m1, m2, m3))
})


test_that("can get generalised array dimensions", {
  expect_equal(dim2(1:6), 6)
  expect_equal(dim2(matrix(1:6, 2, 3)), c(2, 3))
})


test_that("can get generalised array names", {
  v <- 1:6
  m <- matrix(v, 2, 3)
  expect_null(dimnames2(v))
  expect_null(dimnames2(m))

  names(v) <- letters[1:6]
  dimnames(m) <- list(letters[1:2], letters[3:5])
  expect_equal(dimnames2(v), list(letters[1:6]))
  expect_equal(dimnames2(m), list(letters[1:2], letters[3:5]))
})


test_that("can test if things are the same", {
  expect_true(all_same(integer()))
  expect_true(all_same(1))
  expect_true(all_same(c(1, 1)))
  expect_true(all_same(rep(1, 10)))
  expect_false(all_same(c(1, 1, 2, 1)))
})


test_that("can get near matches", {
  x <- c("apples", "applez", "appell", "applly")
  expect_equal(
    near_match("apple", x),
    c("apples", "applez", "appell", "applly"))
  expect_equal(
    near_match("apple", x, 1),
    c("apples", "applez"))
  expect_equal(
    near_match("apple", x, 2, 3),
    c("apples", "applez", "appell"))
})


test_that("can report on package status", {
  skip_if_not_installed("mockery")
  mock_loaded <- mockery::mock(c("a", "b", "c"), c("a", "c"), "a")
  mock_pkgs <- mockery::mock("b", cycle = TRUE)

  mockery::stub(suggested_package_status, "loadedNamespaces", mock_loaded)
  mockery::stub(suggested_package_status, ".packages", mock_pkgs)

  expect_equal(
    suggested_package_status(c("a", "b", "c")),
    c(a = "loaded", b = "loaded", c = "loaded"))
  mockery::expect_called(mock_loaded, 1)
  mockery::expect_called(mock_pkgs, 0)

  expect_equal(
    suggested_package_status(c("a", "b", "c")),
    c(a = "loaded", b = "installed", c = "loaded"))
  mockery::expect_called(mock_loaded, 2)
  mockery::expect_called(mock_pkgs, 1)

  expect_equal(
    suggested_package_status(c("a", "b", "c")),
    c(a = "loaded", b = "installed", c = "missing"))
  mockery::expect_called(mock_loaded, 3)
  mockery::expect_called(mock_pkgs, 2)
})


test_that("can poll for callr result", {
  skip_if_not_installed("mockery")
  rs <- list(is_alive = mockery::mock(TRUE, TRUE, FALSE),
             get_result = mockery::mock(1))
  expect_equal(
    callr_safe_result(rs, 1, 0.001),
    1)
  mockery::expect_called(rs$is_alive, 3)
  mockery::expect_called(rs$get_result, 1)
})
