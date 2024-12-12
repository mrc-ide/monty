test_that("xoshiro output agrees with reference data", {
  obj <- monty_rng_create(seed = 42)
  res <- test_xoshiro_run(obj)
  len <- 8
  s <- matrix(monty_rng_state(obj), len)[rev(seq_len(len)), ]
  s_str <- apply(s, 2, paste, collapse = "")
  cmp <- readLines("xoshiro-ref/xoshiro256plus")
  expect_equal(c(res, s_str), cmp)
})
