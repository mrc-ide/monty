test_that("can select sensible values for progress", {
  withr::with_options(list(mcstate2.progress = TRUE), {
    expect_false(show_progress_bar(FALSE))
    expect_true(show_progress_bar(TRUE))
    expect_true(show_progress_bar(NULL))
  })

  withr::with_options(list(mcstate2.progress = FALSE), {
    expect_false(show_progress_bar(FALSE))
    expect_true(show_progress_bar(TRUE))
    expect_false(show_progress_bar(NULL))
  })

  withr::with_options(list(mcstate2.progress = NULL), {
    expect_false(show_progress_bar(FALSE))
    expect_true(show_progress_bar(TRUE))
    expect_true(show_progress_bar(NULL))
  })
})
