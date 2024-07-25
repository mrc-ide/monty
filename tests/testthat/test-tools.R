test_that("can trace calls to random number functions", {
  f <- function(n) {
    for (i in seq_len(n)) {
      runif(1)
    }
  }

  expect_silent(with_trace_random(f(0)))

  msg <- capture_messages(with_trace_random(f(2)))
  expect_length(msg, 3)
  expect_match(msg[[1]], "Detected call to 'runif()'", fixed = TRUE)
  expect_match(msg[[2]], "Detected call to 'runif()'", fixed = TRUE)
  expect_match(msg[[3]], "A total of 2 calls to rng-using functions found")

  msg <- capture_messages(with_trace_random(f(10)))
  expect_length(msg, 6)
  expect_match(msg[[1]], "Detected call to 'runif()'", fixed = TRUE)
  expect_match(msg[[6]], "A total of 10 calls to rng-using functions found")
})


test_that("can show stack on print", {
  f <- function(n) {
    for (i in seq_len(n)) {
      runif(42)
    }
  }

  expect_silent(with_trace_random(f(0), show_stack = TRUE))

  msg <- capture_messages(with_trace_random(f(1), show_stack = TRUE))
  expect_length(msg, 3)
  expect_match(msg[[1]], "Detected call to 'runif()'", fixed = TRUE)
  expect_match(msg[[2]], "runif(42)", fixed = TRUE)
  expect_match(msg[[3]], "A total of 1 call to rng-using functions found")
})
