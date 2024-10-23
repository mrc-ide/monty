// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// random.cpp
SEXP monty_rng_alloc(cpp11::sexp r_seed, int n_streams, bool deterministic, bool is_float);
extern "C" SEXP _monty_monty_rng_alloc(SEXP r_seed, SEXP n_streams, SEXP deterministic, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_alloc(cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(r_seed), cpp11::as_cpp<cpp11::decay_t<int>>(n_streams), cpp11::as_cpp<cpp11::decay_t<bool>>(deterministic), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
void monty_rng_jump(SEXP ptr, bool is_float);
extern "C" SEXP _monty_monty_rng_jump(SEXP ptr, SEXP is_float) {
  BEGIN_CPP11
    monty_rng_jump(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float));
    return R_NilValue;
  END_CPP11
}
// random.cpp
void monty_rng_long_jump(SEXP ptr, bool is_float);
extern "C" SEXP _monty_monty_rng_long_jump(SEXP ptr, SEXP is_float) {
  BEGIN_CPP11
    monty_rng_long_jump(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float));
    return R_NilValue;
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_random_real(SEXP ptr, int n, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_random_real(SEXP ptr, SEXP n, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_random_real(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_random_normal(SEXP ptr, int n, int n_threads, std::string algorithm, bool is_float);
extern "C" SEXP _monty_monty_rng_random_normal(SEXP ptr, SEXP n, SEXP n_threads, SEXP algorithm, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_random_normal(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<std::string>>(algorithm), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_uniform(SEXP ptr, int n, cpp11::doubles r_min, cpp11::doubles r_max, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_uniform(SEXP ptr, SEXP n, SEXP r_min, SEXP r_max, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_uniform(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_min), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_max), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_exponential_rate(SEXP ptr, int n, cpp11::doubles r_rate, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_exponential_rate(SEXP ptr, SEXP n, SEXP r_rate, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_exponential_rate(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_rate), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_exponential_mean(SEXP ptr, int n, cpp11::doubles r_mean, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_exponential_mean(SEXP ptr, SEXP n, SEXP r_mean, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_exponential_mean(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_mean), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_normal(SEXP ptr, int n, cpp11::doubles r_mean, cpp11::doubles r_sd, int n_threads, std::string algorithm, bool is_float);
extern "C" SEXP _monty_monty_rng_normal(SEXP ptr, SEXP n, SEXP r_mean, SEXP r_sd, SEXP n_threads, SEXP algorithm, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_normal(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_mean), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_sd), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<std::string>>(algorithm), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_binomial(SEXP ptr, int n, cpp11::doubles r_size, cpp11::doubles r_prob, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_binomial(SEXP ptr, SEXP n, SEXP r_size, SEXP r_prob, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_binomial(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_size), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_prob), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_beta_binomial_ab(SEXP ptr, int n, cpp11::doubles r_size, cpp11::doubles r_a, cpp11::doubles r_b, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_beta_binomial_ab(SEXP ptr, SEXP n, SEXP r_size, SEXP r_a, SEXP r_b, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_beta_binomial_ab(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_size), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_a), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_b), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_beta_binomial_prob(SEXP ptr, int n, cpp11::doubles r_size, cpp11::doubles r_prob, cpp11::doubles r_rho, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_beta_binomial_prob(SEXP ptr, SEXP n, SEXP r_size, SEXP r_prob, SEXP r_rho, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_beta_binomial_prob(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_size), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_prob), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_rho), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_negative_binomial_prob(SEXP ptr, int n, cpp11::doubles r_size, cpp11::doubles r_prob, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_negative_binomial_prob(SEXP ptr, SEXP n, SEXP r_size, SEXP r_prob, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_negative_binomial_prob(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_size), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_prob), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_negative_binomial_mu(SEXP ptr, int n, cpp11::doubles r_size, cpp11::doubles r_mu, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_negative_binomial_mu(SEXP ptr, SEXP n, SEXP r_size, SEXP r_mu, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_negative_binomial_mu(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_size), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_mu), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_hypergeometric(SEXP ptr, int n, cpp11::doubles r_n1, cpp11::doubles r_n2, cpp11::doubles r_k, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_hypergeometric(SEXP ptr, SEXP n, SEXP r_n1, SEXP r_n2, SEXP r_k, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_hypergeometric(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_n1), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_n2), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_k), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_gamma_scale(SEXP ptr, int n, cpp11::doubles r_shape, cpp11::doubles r_scale, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_gamma_scale(SEXP ptr, SEXP n, SEXP r_shape, SEXP r_scale, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_gamma_scale(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_shape), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_scale), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_gamma_rate(SEXP ptr, int n, cpp11::doubles r_shape, cpp11::doubles r_rate, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_gamma_rate(SEXP ptr, SEXP n, SEXP r_shape, SEXP r_rate, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_gamma_rate(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_shape), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_rate), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_poisson(SEXP ptr, int n, cpp11::doubles r_lambda, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_poisson(SEXP ptr, SEXP n, SEXP r_lambda, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_poisson(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_lambda), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_cauchy(SEXP ptr, int n, cpp11::doubles r_location, cpp11::doubles r_scale, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_cauchy(SEXP ptr, SEXP n, SEXP r_location, SEXP r_scale, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_cauchy(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_location), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_scale), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_beta(SEXP ptr, int n, cpp11::doubles r_a, cpp11::doubles r_b, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_beta(SEXP ptr, SEXP n, SEXP r_a, SEXP r_b, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_beta(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_a), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_b), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_multinomial(SEXP ptr, int n, cpp11::doubles r_size, cpp11::doubles r_prob, int n_threads, bool is_float);
extern "C" SEXP _monty_monty_rng_multinomial(SEXP ptr, SEXP n, SEXP r_size, SEXP r_prob, SEXP n_threads, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_multinomial(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_size), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_prob), cpp11::as_cpp<cpp11::decay_t<int>>(n_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random.cpp
cpp11::sexp monty_rng_state(SEXP ptr, bool is_float);
extern "C" SEXP _monty_monty_rng_state(SEXP ptr, SEXP is_float) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_state(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr), cpp11::as_cpp<cpp11::decay_t<bool>>(is_float)));
  END_CPP11
}
// random2.cpp
cpp11::doubles cpp_monty_random_real(SEXP ptr);
extern "C" SEXP _monty_cpp_monty_random_real(SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_monty_random_real(cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr)));
  END_CPP11
}
// random2.cpp
cpp11::doubles cpp_monty_random_binomial(cpp11::doubles r_size, cpp11::doubles r_prob, SEXP ptr);
extern "C" SEXP _monty_cpp_monty_random_binomial(SEXP r_size, SEXP r_prob, SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_monty_random_binomial(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_size), cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_prob), cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr)));
  END_CPP11
}
// random2.cpp
cpp11::doubles cpp_monty_random_exponential_rate(cpp11::doubles r_rate, SEXP ptr);
extern "C" SEXP _monty_cpp_monty_random_exponential_rate(SEXP r_rate, SEXP ptr) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_monty_random_exponential_rate(cpp11::as_cpp<cpp11::decay_t<cpp11::doubles>>(r_rate), cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptr)));
  END_CPP11
}
// rng_pointer.cpp
cpp11::sexp monty_rng_pointer_init(int n_streams, cpp11::sexp seed, int long_jump, std::string algorithm);
extern "C" SEXP _monty_monty_rng_pointer_init(SEXP n_streams, SEXP seed, SEXP long_jump, SEXP algorithm) {
  BEGIN_CPP11
    return cpp11::as_sexp(monty_rng_pointer_init(cpp11::as_cpp<cpp11::decay_t<int>>(n_streams), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(seed), cpp11::as_cpp<cpp11::decay_t<int>>(long_jump), cpp11::as_cpp<cpp11::decay_t<std::string>>(algorithm)));
  END_CPP11
}
// rng_pointer.cpp
void monty_rng_pointer_sync(cpp11::environment obj, std::string algorithm);
extern "C" SEXP _monty_monty_rng_pointer_sync(SEXP obj, SEXP algorithm) {
  BEGIN_CPP11
    monty_rng_pointer_sync(cpp11::as_cpp<cpp11::decay_t<cpp11::environment>>(obj), cpp11::as_cpp<cpp11::decay_t<std::string>>(algorithm));
    return R_NilValue;
  END_CPP11
}
// rng_pointer.cpp
double test_rng_pointer_get(cpp11::environment obj, int n_streams);
extern "C" SEXP _monty_test_rng_pointer_get(SEXP obj, SEXP n_streams) {
  BEGIN_CPP11
    return cpp11::as_sexp(test_rng_pointer_get(cpp11::as_cpp<cpp11::decay_t<cpp11::environment>>(obj), cpp11::as_cpp<cpp11::decay_t<int>>(n_streams)));
  END_CPP11
}
// test_rng.cpp
std::vector<std::string> test_xoshiro_run(cpp11::environment obj);
extern "C" SEXP _monty_test_xoshiro_run(SEXP obj) {
  BEGIN_CPP11
    return cpp11::as_sexp(test_xoshiro_run(cpp11::as_cpp<cpp11::decay_t<cpp11::environment>>(obj)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_monty_cpp_monty_random_binomial",         (DL_FUNC) &_monty_cpp_monty_random_binomial,         3},
    {"_monty_cpp_monty_random_exponential_rate", (DL_FUNC) &_monty_cpp_monty_random_exponential_rate, 2},
    {"_monty_cpp_monty_random_real",             (DL_FUNC) &_monty_cpp_monty_random_real,             1},
    {"_monty_monty_rng_alloc",                   (DL_FUNC) &_monty_monty_rng_alloc,                   4},
    {"_monty_monty_rng_beta",                    (DL_FUNC) &_monty_monty_rng_beta,                    6},
    {"_monty_monty_rng_beta_binomial_ab",        (DL_FUNC) &_monty_monty_rng_beta_binomial_ab,        7},
    {"_monty_monty_rng_beta_binomial_prob",      (DL_FUNC) &_monty_monty_rng_beta_binomial_prob,      7},
    {"_monty_monty_rng_binomial",                (DL_FUNC) &_monty_monty_rng_binomial,                6},
    {"_monty_monty_rng_cauchy",                  (DL_FUNC) &_monty_monty_rng_cauchy,                  6},
    {"_monty_monty_rng_exponential_mean",        (DL_FUNC) &_monty_monty_rng_exponential_mean,        5},
    {"_monty_monty_rng_exponential_rate",        (DL_FUNC) &_monty_monty_rng_exponential_rate,        5},
    {"_monty_monty_rng_gamma_rate",              (DL_FUNC) &_monty_monty_rng_gamma_rate,              6},
    {"_monty_monty_rng_gamma_scale",             (DL_FUNC) &_monty_monty_rng_gamma_scale,             6},
    {"_monty_monty_rng_hypergeometric",          (DL_FUNC) &_monty_monty_rng_hypergeometric,          7},
    {"_monty_monty_rng_jump",                    (DL_FUNC) &_monty_monty_rng_jump,                    2},
    {"_monty_monty_rng_long_jump",               (DL_FUNC) &_monty_monty_rng_long_jump,               2},
    {"_monty_monty_rng_multinomial",             (DL_FUNC) &_monty_monty_rng_multinomial,             6},
    {"_monty_monty_rng_negative_binomial_mu",    (DL_FUNC) &_monty_monty_rng_negative_binomial_mu,    6},
    {"_monty_monty_rng_negative_binomial_prob",  (DL_FUNC) &_monty_monty_rng_negative_binomial_prob,  6},
    {"_monty_monty_rng_normal",                  (DL_FUNC) &_monty_monty_rng_normal,                  7},
    {"_monty_monty_rng_pointer_init",            (DL_FUNC) &_monty_monty_rng_pointer_init,            4},
    {"_monty_monty_rng_pointer_sync",            (DL_FUNC) &_monty_monty_rng_pointer_sync,            2},
    {"_monty_monty_rng_poisson",                 (DL_FUNC) &_monty_monty_rng_poisson,                 5},
    {"_monty_monty_rng_random_normal",           (DL_FUNC) &_monty_monty_rng_random_normal,           5},
    {"_monty_monty_rng_random_real",             (DL_FUNC) &_monty_monty_rng_random_real,             4},
    {"_monty_monty_rng_state",                   (DL_FUNC) &_monty_monty_rng_state,                   2},
    {"_monty_monty_rng_uniform",                 (DL_FUNC) &_monty_monty_rng_uniform,                 6},
    {"_monty_test_rng_pointer_get",              (DL_FUNC) &_monty_test_rng_pointer_get,              2},
    {"_monty_test_xoshiro_run",                  (DL_FUNC) &_monty_test_xoshiro_run,                  1},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_monty(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
