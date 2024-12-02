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


void set_dimensions(size_t n_samples, size_t n_streams, cpp11::sexp ptr, cpp11::sexp ret) {
  if (preserve_stream_dimension(n_streams, ptr)) {
    ret.attr("dim") = cpp11::writable::integers{static_cast<int>(n_samples), static_cast<int>(n_streams)};
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
    for (size_t j = 0; j < n_samples; ++j) {
      auto& state_i = rng->state(i);
      auto y_i = y + n_samples * i;
      y_i[j] = fn(state_i, a[i], b[i]);
    }
  }
  set_dimensions(n_samples, n_streams, ptr, r_y);
  return r_y;
}

// Real functions that we export
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
cpp11::doubles cpp_monty_random_real(cpp11::sexp ptr) {
  const auto fn = [](auto& state) { return monty::random::random_real<double>(state); };
  return monty_random_sample_1_0(fn, ptr, "random_real");
}

[[cpp11::register]]
cpp11::doubles cpp_monty_random_n_real(size_t n_samples, cpp11::sexp ptr) {
  const auto fn = [](auto& state) { return monty::random::random_real<double>(state); };
  return monty_random_sample_n_0(fn, n_samples, ptr, "random_real");
}

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
