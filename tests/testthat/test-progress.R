test_that("can select sensible values for progress", {
  withr::with_options(list(monty.progress = TRUE), {
    expect_equal(show_progress_bar(FALSE), "none")
    expect_equal(show_progress_bar(TRUE), "fancy")
    expect_equal(show_progress_bar(NULL), "fancy")
    expect_equal(show_progress_bar("none"), "none")
    expect_equal(show_progress_bar("fancy"), "fancy")
    expect_equal(show_progress_bar("simple"), "simple")
  })

  withr::with_options(list(monty.progress = FALSE), {
    expect_equal(show_progress_bar(FALSE), "none")
    expect_equal(show_progress_bar(TRUE), "fancy")
    expect_equal(show_progress_bar(NULL), "none")
  })

  withr::with_options(list(monty.progress = NULL), {
    expect_equal(show_progress_bar(FALSE), "none")
    expect_equal(show_progress_bar(TRUE), "fancy")
    expect_equal(show_progress_bar(NULL), "fancy")
  })
})


test_that("null progress bar does nothing", {
  p <- progress_bar(10, 10, FALSE)(1)
  expect_silent(p(1))
})


test_that("can format overall progress", {
  withr::local_options(cli.num_colors = 1)
  overall <- progress_overall(4, 100, TRUE, FALSE)
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
  overall <- progress_overall(1, 100, TRUE, FALSE)
  expect_equal(overall(0), "")
  expect_equal(overall(50), "")
  expect_equal(overall(100), "")
})


test_that("overall progress is empty if disabled", {
  overall <- progress_overall(4, 100, FALSE, FALSE)
  expect_equal(overall(0), "")
  expect_equal(overall(50), "")
  expect_equal(overall(100), "")
})


test_that("can format fancy", {
  f <- progress_bar_fancy(4, 100, TRUE)
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
  mock_fancy <- mockery::mock()
  mockery::stub(progress_bar, "progress_bar_none", mock_null)
  mockery::stub(progress_bar, "progress_bar_fancy", mock_fancy)

  progress_bar(4, 100, FALSE, TRUE)
  mockery::expect_called(mock_null, 1)
  mockery::expect_called(mock_fancy, 0)
  expect_equal(mockery::mock_args(mock_null)[[1]], list())

  progress_bar(4, 100, TRUE, TRUE)
  mockery::expect_called(mock_null, 1)
  mockery::expect_called(mock_fancy, 1)
  expect_equal(mockery::mock_args(mock_fancy)[[1]],
               list(4, 100, TRUE, FALSE))
})


test_that("can create a simple progress bar", {
  pb <- progress_bar_simple(104, 5)
  p <- pb(1)
  expect_message(p(10), "MONTY-PROGRESS: chain: 1, step: 10")
  expect_no_message(p(11))
  expect_message(p(36), "MONTY-PROGRESS: chain: 1, step: 36")
  expect_message(p(102), "MONTY-PROGRESS: chain: 1, step: 102")
  expect_no_message(p(103))
  expect_message(p(104), "MONTY-PROGRESS: chain: 1, step: 104")
})


test_that("can parse messages produced by simple progress bar", {
  expect_equal(
    parse_progress_bar_simple("MONTY-PROGRESS: chain: 2, step: 36"),
    list(chain_id = 2, step = 36))

  expect_equal(
    parse_progress_bar_simple(c("MONTY-PROGRESS: chain: 2, step: 36",
                                "other text",
                                "MONTY-PROGRESS: chain: 2, step: 243",
                                "chain: 2, step: 200")),
    list(chain_id = 2, step = 243))

  expect_null(parse_progress_bar_simple(NULL))
  expect_null(parse_progress_bar_simple(character()))
  expect_null(parse_progress_bar_simple("other text"))
})
