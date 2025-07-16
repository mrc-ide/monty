#include <cpp11/doubles.hpp>
#include <cpp11/external_pointer.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/raws.hpp>

#include <monty/r/random.hpp>
#include <monty/random/random.hpp>
#include <monty/utils.hpp>

using default_rng64 = monty::random::prng<monty::random::generator<double>>;

template <typename T>
T* safely_read_externalptr(cpp11::sexp ptr, const char * context) {
  if (!R_ExternalPtrAddr(ptr)) {
    cpp11::stop("Pointer has been serialised, cannot continue safely (%s)",
                context);
  }
  return cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
}

struct input {
  const double * const data;
  size_t len;
  bool fixed;

  input(cpp11::doubles r_data, size_t expected, const char *name) :
    data(REAL(r_data)), len(r_data.size()), fixed(len == 1) {
    if (!fixed && len != expected) {
      if (expected == 1) {
        cpp11::stop("Expected '%s' to have length %d, not %d",
                    name, expected, len);
      } else {
        cpp11::stop("Expected '%s' to have length %d or 1, not %d",
                    name, expected, len);
      }
    }
  }

  auto operator[](size_t i) const {
    return data[fixed ? 0 : i];
  }
};

struct input_array {
  const double * const data;
  size_t len;
  bool fixed;

  input_array(cpp11::doubles r_data, size_t expected, const char *name) :
    data(REAL(r_data)) {
    const auto dim = r_data.attr("dim");
    if (dim == R_NilValue) {
      fixed = true;
      size_ = r_data.size();
    } else {
      if (Rf_length(dim) != 2) {
        cpp11::stop("Expected '%s' to be a matrix", name);
      }
      size_ = INTEGER(dim)[0];
      const auto ncol = INTEGER(dim)[1];
      fixed = ncol == 1;
      if (!fixed && static_cast<size_t>(ncol) != expected) {
        if (expected == 1) {
          cpp11::stop("Expected '%s' to have 1 column, not %d", name, ncol);
        } else {
          cpp11::stop("Expected '%s' to have %d or 1 columns, not %d",
                      name, static_cast<int>(expected), ncol);
        }
      }
    }
  }

  size_t size() const {
    return size_;
  }

  auto operator[](size_t i) const {
    return data + (fixed ? 0 : i * size_);
  }
private:
  size_t size_;
};

bool preserve_stream_dimension(size_t n, cpp11::sexp ptr) {
  return n > 1 || INTEGER(ptr.attr("preserve_stream_dimension"))[0] == 1;
}

// Generic sampler functions for 0, 1, 2, ... parameters
template <typename Fn>
cpp11::doubles monty_random_sample_1_0(Fn fn, cpp11::sexp ptr,
                                       const char * name_distribution) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_streams);

  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    auto &state = rng->state(i);
    y[i] = fn(state);
  }

  return r_y;
}

template <typename Fn>
cpp11::doubles monty_random_sample_1_1(Fn fn, cpp11::sexp ptr,
                                       const char * name_distribution,
                                       cpp11::doubles r_a,
                                       const char * name_a) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  const auto a = input(r_a, n_streams, name_a);
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_streams);

  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    auto &state = rng->state(i);
    y[i] = fn(state, a[i]);
  }

  return r_y;
}

template <typename Fn>
cpp11::doubles monty_random_sample_1_2(Fn fn, cpp11::sexp ptr,
                                       const char * name_distribution,
                                       cpp11::doubles r_a, cpp11::doubles r_b,
                                       const char * name_a, const char * name_b) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  auto a = input(r_a, n_streams, name_a);
  auto b = input(r_b, n_streams, name_b);
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_streams);

  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    auto &state = rng->state(i);
    y[i] = fn(state, a[i], b[i]);
  }

  return r_y;
}

template <typename Fn>
cpp11::doubles monty_random_sample_1_3(Fn fn, cpp11::sexp ptr,
                                       const char * name_distribution,
                                       cpp11::doubles r_a,
                                       cpp11::doubles r_b,
                                       cpp11::doubles r_c,
                                       const char * name_a,
                                       const char * name_b,
                                       const char * name_c) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  auto a = input(r_a, n_streams, name_a);
  auto b = input(r_b, n_streams, name_b);
  auto c = input(r_c, n_streams, name_c);
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_streams);

  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    auto &state = rng->state(i);
    y[i] = fn(state, a[i], b[i], c[i]);
  }

  return r_y;
}

template <typename Fn>
cpp11::doubles monty_random_sample_1_4(Fn fn, cpp11::sexp ptr,
                                       const char * name_distribution,
                                       cpp11::doubles r_a,
                                       cpp11::doubles r_b,
                                       cpp11::doubles r_c,
                                       cpp11::doubles r_d,
                                       const char * name_a,
                                       const char * name_b,
                                       const char * name_c,
                                       const char * name_d) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  auto a = input(r_a, n_streams, name_a);
  auto b = input(r_b, n_streams, name_b);
  auto c = input(r_c, n_streams, name_c);
  auto d = input(r_d, n_streams, name_d);
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_streams);

  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    auto &state = rng->state(i);
    y[i] = fn(state, a[i], b[i], c[i], d[i]);
  }

  return r_y;
}

void set_dimensions(size_t n_samples, size_t n_streams, cpp11::sexp ptr, cpp11::sexp ret) {
  if (preserve_stream_dimension(n_streams, ptr)) {
    ret.attr("dim") = cpp11::writable::integers{static_cast<int>(n_samples), static_cast<int>(n_streams)};
  }
}

void set_dimensions(size_t n_state, size_t n_samples, size_t n_streams, cpp11::sexp ptr, cpp11::sexp ret) {
  if (preserve_stream_dimension(n_streams, ptr)) {
    ret.attr("dim") = cpp11::writable::integers{static_cast<int>(n_state), static_cast<int>(n_samples), static_cast<int>(n_streams)};
  } else {
    ret.attr("dim") = cpp11::writable::integers{static_cast<int>(n_state), static_cast<int>(n_samples)};
  }
}

template <typename Fn>
cpp11::doubles monty_random_sample_n_0(Fn fn, size_t n_samples,
                                       cpp11::sexp ptr,
                                       const char * name_distribution) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_samples * n_streams);
  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    for (size_t j = 0; j < n_samples; ++j) {
      auto& state_i = rng->state(i);
      auto y_i = y + n_samples * i;
      y_i[j] = fn(state_i);
    }
  }
  set_dimensions(n_samples, n_streams, ptr, r_y);
  return r_y;
}

template <typename Fn>
cpp11::doubles monty_random_sample_n_1(Fn fn, size_t n_samples,
                                       cpp11::sexp ptr,
                                       const char * name_distribution,
                                       cpp11::doubles r_a,
                                       const char * name_a) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  auto a = input (r_a, n_streams, name_a);
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_samples * n_streams);

  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    for (size_t j = 0; j < n_samples; ++j) {
      auto& state_i = rng->state(i);
      auto y_i = y + n_samples * i;
      y_i[j] = fn(state_i, a[i]);
    }
  }
  set_dimensions(n_samples, n_streams, ptr, r_y);
  return r_y;
}

template <typename Fn>
cpp11::doubles monty_random_sample_n_2(Fn fn, size_t n_samples,
                                       cpp11::sexp ptr,
                                       const char * name_distribution,
                                       cpp11::doubles r_a,
                                       cpp11::doubles r_b,
                                       const char * name_a,
                                       const char * name_b) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  auto a = input(r_a, n_streams, name_a);
  auto b = input(r_b, n_streams, name_b);
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_samples * n_streams);

  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    auto& state_i = rng->state(i);
    for (size_t j = 0; j < n_samples; ++j) {
      auto y_i = y + n_samples * i;
      y_i[j] = fn(state_i, a[i], b[i]);
    }
  }
  set_dimensions(n_samples, n_streams, ptr, r_y);
  return r_y;
}

template <typename Fn>
cpp11::doubles monty_random_sample_n_3(Fn fn, size_t n_samples,
                                       cpp11::sexp ptr,
                                       const char * name_distribution,
                                       cpp11::doubles r_a,
                                       cpp11::doubles r_b,
                                       cpp11::doubles r_c,
                                       const char * name_a,
                                       const char * name_b,
                                       const char * name_c) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  auto a = input(r_a, n_streams, name_a);
  auto b = input(r_b, n_streams, name_b);
  auto c = input(r_c, n_streams, name_c);
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_samples * n_streams);

  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    for (size_t j = 0; j < n_samples; ++j) {
      auto& state_i = rng->state(i);
      auto y_i = y + n_samples * i;
      y_i[j] = fn(state_i, a[i], b[i], c[i]);
    }
  }
  set_dimensions(n_samples, n_streams, ptr, r_y);
  return r_y;
}

template <typename Fn>
cpp11::doubles monty_random_sample_n_4(Fn fn, size_t n_samples,
                                       cpp11::sexp ptr,
                                       const char * name_distribution,
                                       cpp11::doubles r_a,
                                       cpp11::doubles r_b,
                                       cpp11::doubles r_c,
                                       cpp11::doubles r_d,
                                       const char * name_a,
                                       const char * name_b,
                                       const char * name_c,
                                       const char * name_d) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, name_distribution);
  const size_t n_streams = rng->size();
  auto a = input(r_a, n_streams, name_a);
  auto b = input(r_b, n_streams, name_b);
  auto c = input(r_c, n_streams, name_c);
  auto d = input(r_d, n_streams, name_d);
  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_samples * n_streams);

  double * y = REAL(r_y);
  for (size_t i = 0; i < n_streams; ++i) {
    for (size_t j = 0; j < n_samples; ++j) {
      auto& state_i = rng->state(i);
      auto y_i = y + n_samples * i;
      y_i[j] = fn(state_i, a[i], b[i], c[i], d[i]);
    }
  }
  set_dimensions(n_samples, n_streams, ptr, r_y);
  return r_y;
}


template <typename T>
SEXP monty_rng_alloc(cpp11::sexp r_seed, int n_streams, bool deterministic) {
  auto seed = monty::random::r::as_rng_seed<typename T::rng_state>(r_seed);
  T *rng = new T(n_streams, seed, deterministic);
  return cpp11::external_pointer<T>(rng);
}


// Real functions that we export
[[cpp11::register]]
SEXP monty_rng_alloc(cpp11::sexp r_seed, int n_streams, bool deterministic) {
  return monty_rng_alloc<default_rng64>(r_seed, n_streams, deterministic);
}


[[cpp11::register]]
cpp11::sexp cpp_monty_rng_state(cpp11::sexp ptr) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, "state");
  auto state = rng->export_state();
  size_t len = sizeof(typename default_rng64::int_type) * state.size();
  cpp11::writable::raws ret(len);
  std::memcpy(RAW(ret), state.data(), len);
  return ret;
}

[[cpp11::register]]
void cpp_monty_rng_set_state(cpp11::sexp ptr, cpp11::raws r_value) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, "set_state");

  using int_type = typename default_rng64::int_type;
  const auto len = rng->state_size() * sizeof(int_type);
  if ((size_t)r_value.size() != len) {
    cpp11::stop("'value' must be a raw vector of length %d (but was %d)",
                len, r_value.size());
  }
  std::vector<int_type> state(len);
  std::memcpy(state.data(), RAW(r_value), len);
  rng->import_state(state);
}


[[cpp11::register]]
void cpp_monty_rng_jump(cpp11::sexp ptr, int n) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, "jump");
  for (int i = 0; i < n; ++i) {
    rng->jump();
  }
}

[[cpp11::register]]
void cpp_monty_rng_long_jump(cpp11::sexp ptr, int n) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, "long_jump");
  for (int i = 0; i < n; ++i) {
    rng->long_jump();
  }
}

//// 0-arg functions

// real
[[cpp11::register]]
cpp11::doubles cpp_monty_random_real(cpp11::sexp ptr) {
  const auto fn = [](auto& state) { return monty::random::random_real<double>(state); };
  return monty_random_sample_1_0(fn, ptr, "random_real");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_real(size_t n_samples, cpp11::sexp ptr) {
  const auto fn = [](auto& state) { return monty::random::random_real<double>(state); };
  return monty_random_sample_n_0(fn, n_samples, ptr, "random_real");
}

//// 1-arg functions

// exponential_rate
[[cpp11::register]]
cpp11::doubles cpp_monty_random_exponential_rate(cpp11::doubles rate, cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto rate) { return monty::random::exponential_rate<double>(state, rate); };
  return monty_random_sample_1_1(fn, ptr, "exponential_rate",
                                 rate, "rate");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_exponential_rate(size_t n_samples, cpp11::doubles rate, cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto rate) { return monty::random::exponential_rate<double>(state, rate); };
  return monty_random_sample_n_1(fn, n_samples, ptr, "exponential_rate",
                                 rate, "rate");
}

// exponential_mean
[[cpp11::register]]
cpp11::doubles cpp_monty_random_exponential_mean(cpp11::doubles mean, cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto mean) { return monty::random::exponential_mean<double>(state, mean); };
  return monty_random_sample_1_1(fn, ptr, "exponential_mean",
                                 mean, "mean");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_exponential_mean(size_t n_samples, cpp11::doubles mean, cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto mean) { return monty::random::exponential_mean<double>(state, mean); };
  return monty_random_sample_n_1(fn, n_samples, ptr, "exponential_mean",
                                 mean, "mean");
}

// poisson
[[cpp11::register]]
cpp11::doubles cpp_monty_random_poisson(cpp11::doubles lambda, cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto lambda) { return monty::random::poisson<double>(state, lambda); };
  return monty_random_sample_1_1(fn, ptr, "poisson",
                                 lambda, "lambda");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_poisson(size_t n_samples, cpp11::doubles lambda, cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto lambda) { return monty::random::poisson<double>(state, lambda); };
  return monty_random_sample_n_1(fn, n_samples, ptr, "poisson",
                                 lambda, "lambda");
}

//// 2-arg functions
[[cpp11::register]]
cpp11::doubles cpp_monty_random_beta(cpp11::doubles a,
                                     cpp11::doubles b,
                                     cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto a, auto b) { return monty::random::beta<double>(state, a, b); };
  return monty_random_sample_1_2(fn, ptr, "beta", a, b, "a", "b");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_beta(size_t n_samples,
                                       cpp11::doubles a,
                                       cpp11::doubles b,
                                       cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto a, auto b) { return monty::random::beta<double>(state, a, b); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "beta", a, b, "a", "b");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_binomial(cpp11::doubles size,
                                         cpp11::doubles prob,
                                         cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto prob) { return monty::random::binomial<double>(state, size, prob); };
  return monty_random_sample_1_2(fn, ptr, "binomial",
                                 size, prob, "size", "prob");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_binomial(size_t n_samples,
                                           cpp11::doubles size,
                                           cpp11::doubles prob,
                                           cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto prob) { return monty::random::binomial<double>(state, size, prob); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "binomial",
                                 size, prob, "size", "prob");
}

// cauchy
[[cpp11::register]]
cpp11::doubles cpp_monty_random_cauchy(cpp11::doubles location,
                                       cpp11::doubles scale,
                                       cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto location, auto scale) { return monty::random::cauchy<double>(state, location, scale); };
  return monty_random_sample_1_2(fn, ptr, "cauchy",
                                 location, scale, "location", "scale");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_cauchy(size_t n_samples,
                                         cpp11::doubles location,
                                         cpp11::doubles scale,
                                         cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto location, auto scale) { return monty::random::cauchy<double>(state, location, scale); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "cauchy",
                                 location, scale, "location", "scale");
}

// gamma_scale
[[cpp11::register]]
cpp11::doubles cpp_monty_random_gamma_scale(cpp11::doubles shape,
                                            cpp11::doubles scale,
                                            cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto shape, auto scale) { return monty::random::gamma_scale<double>(state, shape, scale); };
  return monty_random_sample_1_2(fn, ptr, "gamma_scale",
                                 shape, scale, "shape", "scale");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_gamma_scale(size_t n_samples,
                                              cpp11::doubles shape,
                                              cpp11::doubles scale,
                                              cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto shape, auto scale) { return monty::random::gamma_scale<double>(state, shape, scale); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "gamma_scale",
                                 shape, scale, "shape", "scale");
}

// gamma_rate
[[cpp11::register]]
cpp11::doubles cpp_monty_random_gamma_rate(cpp11::doubles shape,
                                           cpp11::doubles rate,
                                           cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto shape, auto rate) { return monty::random::gamma_rate<double>(state, shape, rate); };
  return monty_random_sample_1_2(fn, ptr, "gamma_rate",
                                 shape, rate, "shape", "rate");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_gamma_rate(size_t n_samples,
                                             cpp11::doubles shape,
                                             cpp11::doubles rate,
                                             cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto shape, auto rate) { return monty::random::gamma_rate<double>(state, shape, rate); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "gamma_rate",
                                 shape, rate, "shape", "rate");
}

// negative_binomial_prob
[[cpp11::register]]
cpp11::doubles cpp_monty_random_negative_binomial_prob(cpp11::doubles size,
                                                       cpp11::doubles prob,
                                                       cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto prob) { return monty::random::negative_binomial_prob<double>(state, size, prob); };
  return monty_random_sample_1_2(fn, ptr, "negative_binomial_prob",
                                 size, prob, "size", "prob");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_negative_binomial_prob(size_t n_samples,
                                                         cpp11::doubles size,
                                                         cpp11::doubles prob,
                                                         cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto prob) { return monty::random::negative_binomial_prob<double>(state, size, prob); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "negative_binomial_prob",
                                 size, prob, "size", "prob");
}

// negative_binomial_mu
[[cpp11::register]]
cpp11::doubles cpp_monty_random_negative_binomial_mu(cpp11::doubles size,
                                                     cpp11::doubles mu,
                                                     cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto mu) { return monty::random::negative_binomial_mu<double>(state, size, mu); };
  return monty_random_sample_1_2(fn, ptr, "negative_binomial_mu",
                                 size, mu, "size", "mu");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_negative_binomial_mu(size_t n_samples,
                                                       cpp11::doubles size,
                                                       cpp11::doubles mu,
                                                       cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto mu) { return monty::random::negative_binomial_mu<double>(state, size, mu); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "negative_binomial_mu",
                                 size, mu, "size", "mu");
}

// normal
template <typename monty::random::algorithm::normal A>
cpp11::doubles cpp_monty_random_normal(cpp11::doubles mean,
                                       cpp11::doubles sd,
                                       cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto mean, auto sd) { return monty::random::normal<double, A>(state, mean, sd); };
  return monty_random_sample_1_2(fn, ptr, "normal",
                                 mean, sd, "mean", "sd");
}

template <typename monty::random::algorithm::normal A>
cpp11::doubles cpp_monty_random_n_normal(size_t n_samples,
                                         cpp11::doubles mean,
                                         cpp11::doubles sd,
                                         cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto mean, auto sd) { return monty::random::normal<double, A>(state, mean, sd); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "normal",
                                 mean, sd, "mean", "sd");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_normal_box_muller(cpp11::doubles mean,
                                                  cpp11::doubles sd,
                                                  cpp11::sexp ptr) {
  return cpp_monty_random_normal<monty::random::algorithm::normal::box_muller>(mean, sd, ptr);
}


[[cpp11::register]]
cpp11::doubles cpp_monty_random_normal_polar(cpp11::doubles mean,
                                             cpp11::doubles sd,
                                             cpp11::sexp ptr) {
  return cpp_monty_random_normal<monty::random::algorithm::normal::polar>(mean, sd, ptr);
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_normal_ziggurat(cpp11::doubles mean,
                                                cpp11::doubles sd,
                                                cpp11::sexp ptr) {
  return cpp_monty_random_normal<monty::random::algorithm::normal::ziggurat>(mean, sd, ptr);
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_normal_box_muller(size_t n_samples,
                                                    cpp11::doubles mean,
                                                    cpp11::doubles sd,
                                                    cpp11::sexp ptr) {
  return cpp_monty_random_n_normal<monty::random::algorithm::normal::box_muller>(n_samples, mean, sd, ptr);
}


[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_normal_polar(size_t n_samples,
                                               cpp11::doubles mean,
                                               cpp11::doubles sd,
                                               cpp11::sexp ptr) {
  return cpp_monty_random_n_normal<monty::random::algorithm::normal::polar>(n_samples, mean, sd, ptr);
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_normal_ziggurat(size_t n_samples,
                                                  cpp11::doubles mean,
                                                  cpp11::doubles sd,
                                                  cpp11::sexp ptr) {
  return cpp_monty_random_n_normal<monty::random::algorithm::normal::ziggurat>(n_samples, mean, sd, ptr);
}

// uniform
[[cpp11::register]]
cpp11::doubles cpp_monty_random_uniform(cpp11::doubles min,
                                        cpp11::doubles max,
                                        cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto min, auto max) { return monty::random::uniform<double>(state, min, max); };
  return monty_random_sample_1_2(fn, ptr, "uniform",
                                 min, max, "min", "max");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_uniform(size_t n_samples,
                                          cpp11::doubles min,
                                          cpp11::doubles max,
                                          cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto min, auto max) { return monty::random::uniform<double>(state, min, max); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "uniform",
                                 min, max, "min", "max");
}

// weibull
[[cpp11::register]]
cpp11::doubles cpp_monty_random_weibull(cpp11::doubles shape,
                                        cpp11::doubles scale,
                                        cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto shape, auto scale) { return monty::random::weibull<double>(state, shape, scale); };
  return monty_random_sample_1_2(fn, ptr, "weibull",
                                 shape, scale, "shape", "scale");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_weibull(size_t n_samples,
                                          cpp11::doubles shape,
                                          cpp11::doubles scale,
                                          cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto shape, auto scale) { return monty::random::weibull<double>(state, shape, scale); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "weibull",
                                 shape, scale, "shape", "scale");
}

// log_normal
[[cpp11::register]]
cpp11::doubles cpp_monty_random_log_normal(cpp11::doubles meanlog,
                                           cpp11::doubles sdlog,
                                           cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto meanlog, auto sdlog) { return monty::random::log_normal<double>(state, meanlog, sdlog); };
  return monty_random_sample_1_2(fn, ptr, "log_normal",
                                 meanlog, sdlog, "meanlog", "sdlog");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_log_normal(size_t n_samples,
                                             cpp11::doubles meanlog,
                                             cpp11::doubles sdlog,
                                             cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto meanlog, auto sdlog) { return monty::random::log_normal<double>(state, meanlog, sdlog); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "log_normal",
                                 meanlog, sdlog, "meanlog", "sdlog");
}

// zi_poisson
[[cpp11::register]]
cpp11::doubles cpp_monty_random_zi_poisson(cpp11::doubles lambda,
                                           cpp11::doubles pi,
                                           cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto lambda, auto pi) { return monty::random::zi_poisson<double>(state, lambda, pi); };
  return monty_random_sample_1_2(fn, ptr, "zi_poisson",
                                 lambda, pi, "lambda", "pi");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_zi_poisson(size_t n_samples,
                                             cpp11::doubles lambda,
                                             cpp11::doubles pi,
                                             cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto lambda, auto pi) { return monty::random::zi_poisson<double>(state, lambda, pi); };
  return monty_random_sample_n_2(fn, n_samples, ptr, "zi_poisson",
                                 lambda, pi, "lambda", "pi");
}

//// 3-arg functions

// beta_binomial_prob
[[cpp11::register]]
cpp11::doubles cpp_monty_random_beta_binomial_prob(cpp11::doubles size,
                                                   cpp11::doubles prob,
                                                   cpp11::doubles rho,
                                                   cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto prob, auto rho) { return monty::random::beta_binomial_prob<double>(state, size, prob, rho); };
  return monty_random_sample_1_3(fn, ptr, "beta_binomial_prob",
                                 size, prob, rho, "size", "prob", "rho");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_beta_binomial_prob(size_t n_samples,
                                                     cpp11::doubles size,
                                                     cpp11::doubles prob,
                                                     cpp11::doubles rho,
                                                     cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto prob, auto rho) { return monty::random::beta_binomial_prob<double>(state, size, prob, rho); };
  return monty_random_sample_n_3(fn, n_samples, ptr, "beta_binomial_prob",
                                 size, prob, rho, "size", "prob", "rho");
}

// beta_binomial_ab
[[cpp11::register]]
cpp11::doubles cpp_monty_random_beta_binomial_ab(cpp11::doubles size,
                                                 cpp11::doubles a,
                                                 cpp11::doubles b,
                                                 cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto a, auto b) { return monty::random::beta_binomial_ab<double>(state, size, a, b); };
  return monty_random_sample_1_3(fn, ptr, "beta_binomial_ab",
                                 size, a, b, "size", "a", "b");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_beta_binomial_ab(size_t n_samples,
                                                   cpp11::doubles size,
                                                   cpp11::doubles a,
                                                   cpp11::doubles b,
                                                   cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto a, auto b) { return monty::random::beta_binomial_ab<double>(state, size, a, b); };
  return monty_random_sample_n_3(fn, n_samples, ptr, "beta_binomial_ab",
                                 size, a, b, "size", "a", "b");
}

// hypergeometric
[[cpp11::register]]
cpp11::doubles cpp_monty_random_hypergeometric(cpp11::doubles n1,
                                               cpp11::doubles n2,
                                               cpp11::doubles k,
                                               cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto n1, auto n2, auto k) { return monty::random::hypergeometric<double>(state, n1, n2, k); };
  return monty_random_sample_1_3(fn, ptr, "hypergeometric",
                                 n1, n2, k, "n1", "n2", "k");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_hypergeometric(size_t n_samples,
                                                 cpp11::doubles n1,
                                                 cpp11::doubles n2,
                                                 cpp11::doubles k,
                                                 cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto n1, auto n2, auto k) { return monty::random::hypergeometric<double>(state, n1, n2, k); };
  return monty_random_sample_n_3(fn, n_samples, ptr, "hypergeometric",
                                 n1, n2, k, "n1", "n2", "k");
}

// zi_negative_binomial_prob
[[cpp11::register]]
cpp11::doubles cpp_monty_random_zi_negative_binomial_prob(cpp11::doubles size,
                                                          cpp11::doubles prob,
                                                          cpp11::doubles pi,
                                                          cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto prob, auto pi) { return monty::random::zi_negative_binomial_prob<double>(state, size, prob, pi); };
  return monty_random_sample_1_3(fn, ptr, "zi_negative_binomial_prob",
                                 size, prob, pi, "size", "prob", "pi");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_zi_negative_binomial_prob(size_t n_samples,
                                                            cpp11::doubles size,
                                                            cpp11::doubles prob,
                                                            cpp11::doubles pi,
                                                            cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto prob, auto pi) { return monty::random::zi_negative_binomial_prob<double>(state, size, prob, pi); };
  return monty_random_sample_n_3(fn, n_samples, ptr, "zi_negative_binomial_prob",
                                 size, prob, pi, "size", "prob", "pi");
}

// zi_negative_binomial_mu
[[cpp11::register]]
cpp11::doubles cpp_monty_random_zi_negative_binomial_mu(cpp11::doubles size,
                                                        cpp11::doubles mu,
                                                        cpp11::doubles pi,
                                                        cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto mu, auto pi) { return monty::random::zi_negative_binomial_mu<double>(state, size, mu, pi); };
  return monty_random_sample_1_3(fn, ptr, "zi_negative_binomial_mu",
                                 size, mu, pi, "size", "mu", "pi");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_zi_negative_binomial_mu(size_t n_samples,
                                                          cpp11::doubles size,
                                                          cpp11::doubles mu,
                                                          cpp11::doubles pi,
                                                          cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto size, auto mu, auto pi) { return monty::random::zi_negative_binomial_mu<double>(state, size, mu, pi); };
  return monty_random_sample_n_3(fn, n_samples, ptr, "zi_negative_binomial_mu",
                                 size, mu, pi, "size", "mu", "pi");
}

//// 4-arg functions

// truncated_normal
[[cpp11::register]]
cpp11::doubles cpp_monty_random_truncated_normal(cpp11::doubles mean,
                                                 cpp11::doubles sd,
                                                 cpp11::doubles min,
                                                 cpp11::doubles max,
                                                 cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto mean, auto sd, auto min, auto max) { return monty::random::truncated_normal<double>(state, mean, sd, min, max); };
  return monty_random_sample_1_4(fn, ptr, "truncated_normal",
                                 mean, sd, min, max, "mean", "sd", "min", "max");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_truncated_normal(size_t n_samples,
                                                   cpp11::doubles mean,
                                                   cpp11::doubles sd,
                                                   cpp11::doubles min,
                                                   cpp11::doubles max,
                                                   cpp11::sexp ptr) {
  const auto fn = [](auto& state, auto mean, auto sd, auto min, auto max) { return monty::random::truncated_normal<double>(state, mean, sd, min, max); };
  return monty_random_sample_n_4(fn, n_samples, ptr, "truncated_normal",
                                 mean, sd, min, max, "mean", "sd", "min", "max");
}

// Other
// multinomial
[[cpp11::register]]
cpp11::doubles cpp_monty_random_multinomial(cpp11::doubles r_size,
                                            cpp11::doubles r_prob,
                                            cpp11::sexp ptr) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, "multinomial");
  const size_t n_streams = rng->size();
  auto size = input(r_size, n_streams, "size");
  auto prob = input_array(r_prob, n_streams, "prob");
  const auto len = prob.size();

  cpp11::writable::doubles r_y = cpp11::writable::doubles(n_streams * len);
  double * y = REAL(r_y);

  for (size_t i = 0; i < n_streams; ++i) {
    auto &state = rng->state(i);
    monty::random::multinomial<double>(state, size[i], prob[i], len, y + i * len);
  }

  set_dimensions(len, n_streams, ptr, r_y);

  return r_y;
}


[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_multinomial(size_t n_samples,
                                              cpp11::doubles r_size,
                                              cpp11::doubles r_prob,
                                              cpp11::sexp ptr) {
  auto * rng = safely_read_externalptr<default_rng64>(ptr, "multinomial");
  const size_t n_streams = rng->size();
  auto size = input(r_size, n_streams, "size");
  auto prob = input_array(r_prob, n_streams, "prob");
  const auto len = prob.size();

  cpp11::writable::doubles r_y =
    cpp11::writable::doubles(n_samples * n_streams * len);
  double * y = REAL(r_y);

  for (size_t i = 0; i < n_streams; ++i) {
    auto &state = rng->state(i);
    for (size_t j = 0; j < n_samples; ++j) {
      auto y_ij = y + len * (n_samples * i + j);
      monty::random::multinomial<double>(state, size[i], prob[i], len, y_ij);
    }
  }

  set_dimensions(len, n_samples, n_streams, ptr, r_y);

  return r_y;
}
