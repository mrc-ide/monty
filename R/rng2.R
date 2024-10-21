monty_random_alloc <- function(n_streams = 1L, seed = NULL,
                               deterministic = FALSE) {
  float <- FALSE
  ptr <- monty_rng_alloc(seed, n_streams, deterministic, float)
  ret <- list(ptr = ptr, n_streams = n_streams, deterministic = deterministic)
  class(ret) <- "monty_random_state"
  ret
}


monty_random_real <- function(state) {
  cpp_monty_random_real(state$ptr)
}


monty_random_binomial <- function(size, prob, state) {
  cpp_monty_random_binomial(size, prob, state$ptr)
}


monty_random_exponential_rate <- function(rate, state) {
  cpp_monty_random_exponential_rate(rate, state$ptr)
}
