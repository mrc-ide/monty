test_that("xoshiro output agrees with reference data", {
  path <- "xoshiro-ref"
  status <- list()
  name <- "xoshiro256plus"

  obj <- monty_rng_create(seed = 42)
  res <- test_xoshiro_run(obj, name)
  len <- 8
  s <- matrix(monty_rng_state(obj), len)[rev(seq_len(len)), ]
  s_str <- apply(s, 2, paste, collapse = "")
  cmp <- readLines(sprintf("%s/%s", path, name))
  expect_equal(c(res, s_str), cmp)
})
