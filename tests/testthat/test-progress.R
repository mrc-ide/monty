test_that("can select sensible values for progress", {
  withr::local_envvar(TESTTHAT = FALSE)
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
  p <- progress_bar(10, 10, FALSE)
  expect_silent(p$update(1, 1))
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
  f <- progress_bar_fancy(4, 100, TRUE)$update
  id <- environment(f)$id
  e <- environment(f)$e

  mock_update <- mockery::mock()
  mockery::stub(f, "cli::cli_progress_update", mock_update)
  f(1, 5)
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
  p <- progress_bar_simple(104, 5)$update
  expect_message(p(1, 10), "MONTY-PROGRESS: chain: 1, step: 10")
  expect_no_message(p(1, 11))
  expect_message(p(1, 36), "MONTY-PROGRESS: chain: 1, step: 36")
  expect_message(p(1, 102), "MONTY-PROGRESS: chain: 1, step: 102")
  expect_no_message(p(1, 103))
  expect_message(p(1, 104), "MONTY-PROGRESS: chain: 1, step: 104")
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


test_that("can fail progress bar nicely", {
  withr::local_options(monty.progress = TRUE)
  env <- list2env(list(n = 0))
  density <- function(x) {
    if (env$n >= 10) {
      stop("some error")
    }
    env$n <- env$n + 1
    dnorm(x, log = TRUE)
  }
  m <- monty_model(list(parameters = "x", density = density))
  s <- monty_sampler_random_walk(vcv = diag(1))

  res <- evaluate_promise(
    tryCatch(monty_sample(m, s, 100, n_chains = 4, initial = 1),
             error = identity))
  expect_match(res$messages, "Sampling stopped at 9 steps after", all = FALSE)
  expect_s3_class(res$result, "simpleError")
  expect_equal(conditionMessage(res$result), "some error")
})
