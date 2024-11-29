#include <cpp11/doubles.hpp>
#include <cpp11/external_pointer.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/raws.hpp>

#include <monty/r/random.hpp>
#include <monty/random/random.hpp>
#include <monty/utils.hpp>

using default_rng64 = monty::random::prng<monty::random::generator<double>>;

template <typename T>
void check_length(T x, size_t len, const char * name) {
  const size_t len_given = Rf_length(x);
  if (len_given != len) {
    cpp11::stop("Expected '%s' to have length %d, not %d",
                name, len, len_given);
  }
}


[[cpp11::register]]
cpp11::doubles cpp_monty_random_real(SEXP ptr) {
  default_rng64 *rng = cpp11::as_cpp<cpp11::external_pointer<default_rng64>>(ptr).get();
  const int n = rng->size();
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n);
  double *y = REAL(r_y);
  for (int i = 0; i < n; ++i) {
    y[i] = monty::random::random_real<double>(rng->state(i));
  }
  return r_y;
}


[[cpp11::register]]
cpp11::doubles cpp_monty_random_real(SEXP ptr) {
  default_rng64 *rng = cpp11::as_cpp<cpp11::external_pointer<default_rng64>>(ptr).get();
  const int n = rng->size();
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n);
  double *y = REAL(r_y);
  for (int i = 0; i < n; ++i) {
    y[i] = monty::random::random_real<double>(rng->state(i));
  }
  return r_y;
}


[[cpp11::register]]
cpp11::doubles cpp_monty_random_binomial(cpp11::doubles r_size,
                                         cpp11::doubles r_prob,
                                         SEXP ptr) {
  default_rng64 *rng = cpp11::as_cpp<cpp11::external_pointer<default_rng64>>(ptr).get();
  const int n = rng->size();
  check_length(r_size, n, "size");
  check_length(r_prob, n, "size");
  const double * size = REAL(r_size);
  const double * prob = REAL(r_prob);

  cpp11::writable::doubles r_y = cpp11::writable::doubles(n);
  double *y = REAL(r_y);
  for (int i = 0; i < n; ++i) {
    auto &state = rng->state(i);
    y[i] = monty::random::binomial<double>(state, size[i], prob[i]);
  }

  return r_y;
}


[[cpp11::register]]
cpp11::doubles cpp_monty_random_exponential_rate(cpp11::doubles r_rate,
                                                 SEXP ptr) {
  default_rng64 *rng = cpp11::as_cpp<cpp11::external_pointer<default_rng64>>(ptr).get();
  const int n = rng->size();
  check_length(r_rate, n, "size");
  const double * rate = REAL(r_rate);

  cpp11::writable::doubles r_y = cpp11::writable::doubles(n);
  double *y = REAL(r_y);
  for (int i = 0; i < n; ++i) {
    auto &state = rng->state(i);
    y[i] = monty::random::exponential_rate<double>(state, rate[i]);
  }

  return r_y;
}
