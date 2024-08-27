test_that("can select sensible values for progress", {
  withr::with_options(list(monty.progress = TRUE), {
    expect_false(show_progress_bar(FALSE))
    expect_true(show_progress_bar(TRUE))
    expect_true(show_progress_bar(NULL))
  })

  withr::with_options(list(monty.progress = FALSE), {
    expect_false(show_progress_bar(FALSE))
    expect_true(show_progress_bar(TRUE))
    expect_false(show_progress_bar(NULL))
  })

  withr::with_options(list(monty.progress = NULL), {
    expect_false(show_progress_bar(FALSE))
    expect_true(show_progress_bar(TRUE))
    expect_true(show_progress_bar(NULL))
  })
})


test_that("null progress bar does nothing", {
  p <- progress_bar(10, 10, FALSE)(1)
  expect_silent(p(1))
})


test_that("can format overall progress", {
  withr::local_options(cli.num_colors = 1)
  overall <- progress_overall(4, 100, TRUE)
  expect_equal(
    overall(c(100, 30, 10, 0)),
    paste0("[",
           cli::symbol$lower_block_8,
           cli::symbol$lower_block_3,
           cli::symbol$lower_block_1,
           cli::symbol$lower_block_1,
           "]"))
  expect_equal(
    overall(c(100, 30, 10, 100)),
    paste0("[",
           cli::symbol$lower_block_8,
           cli::symbol$lower_block_3,
           cli::symbol$lower_block_1,
           cli::symbol$lower_block_8,
           "]"))
})


test_that("overall progress is empty with one chain", {
  overall <- progress_overall(1, 100, TRUE)
  expect_equal(overall(0), "")
  expect_equal(overall(50), "")
  expect_equal(overall(100), "")
})


test_that("overall progress is empty if disabled", {
  overall <- progress_overall(4, 100, FALSE)
  expect_equal(overall(0), "")
  expect_equal(overall(50), "")
  expect_equal(overall(100), "")
})


test_that("can format detail", {
  f <- progress_bar_detail(4, 100, TRUE)
  g <- f(1)
  id <- environment(f)$id
  e <- environment(f)$e

  mock_update <- mockery::mock()
  mockery::stub(g, "cli::cli_progress_update", mock_update)
  g(5)
  mockery::expect_called(mock_update, 1)
  expect_equal(mockery::mock_args(mock_update)[[1]],
               list(id = id, set = 5))
  expect_equal(e$n, c(5, 0, 0, 0))
})


test_that("can create pb", {
  mock_null <- mockery::mock()
  mock_detail <- mockery::mock()
  mockery::stub(progress_bar, "progress_bar_null", mock_null)
  mockery::stub(progress_bar, "progress_bar_detail", mock_detail)

  progress_bar(4, 100, FALSE, TRUE)
  mockery::expect_called(mock_null, 1)
  mockery::expect_called(mock_detail, 0)
  expect_equal(mockery::mock_args(mock_null)[[1]], list())

  progress_bar(4, 100, TRUE, TRUE)
  mockery::expect_called(mock_null, 1)
  mockery::expect_called(mock_detail, 1)
  expect_equal(mockery::mock_args(mock_detail)[[1]],
               list(4, 100, TRUE))
})
