#pragma once

#include <cstring> // memcpy

#include <cpp11/environment.hpp>
#include <cpp11/external_pointer.hpp>
#include <cpp11/list.hpp>
#include <cpp11/raws.hpp>

#include <R_ext/Random.h>

#include "monty/random/generator.hpp"
#include "monty/random/prng.hpp"

namespace monty {
namespace random {
namespace r {

template <typename rng_state_type>
std::vector<typename rng_state_type::int_type> raw_seed(cpp11::raws seed_data) {
  using int_type = typename rng_state_type::int_type;
  constexpr size_t len = sizeof(int_type) * rng_state_type::size();
  if (seed_data.size() == 0 || seed_data.size() % len != 0) {
    cpp11::stop("Expected raw vector of length as multiple of %d for 'seed'",
                len);
  }
  std::vector<int_type> seed(seed_data.size() / sizeof(int_type));
  std::memcpy(seed.data(), RAW(seed_data), seed_data.size());
  return seed;
}

/// Create a seed vector from an R object
///
/// @tparam rng_state_type The random number state type to use; this
/// is used to work out what type of integer is required and how many
/// of them.
///
/// @param r_seed An R object to use as a seed. Valid options are:
/// * a scalar integer (or integer-like number) which we pass to
///   `monty::random::seed`
/// * a vector of raw values, which we take as a serialised vector of
///   integers of appropriate width.
/// * the R value `NULL` (i.e., `R_NilValue`, **not** a C++ `nullptr`),
///   in which case we draw a random integer from R's random number
///   generator and pass that to `monty::random::seed`
template <typename rng_state_type>
std::vector<typename rng_state_type::int_type> as_rng_seed(cpp11::sexp r_seed) {
  using int_type = typename rng_state_type::int_type;
  auto seed_type = TYPEOF(r_seed);
  std::vector<int_type> seed;
  if (seed_type == INTSXP || seed_type == REALSXP) {
    size_t seed_int = cpp11::as_cpp<size_t>(r_seed);
    seed = monty::random::seed_data<rng_state_type>(seed_int);
  } else if (seed_type == RAWSXP) {
    cpp11::raws seed_data = cpp11::as_cpp<cpp11::raws>(r_seed);
    seed = raw_seed<rng_state_type>(seed_data);
  } else if (seed_type == NILSXP) {
    GetRNGstate();
    size_t seed_int =
      std::ceil(std::abs(::unif_rand()) * std::numeric_limits<size_t>::max());
    PutRNGstate();
    seed = monty::random::seed_data<rng_state_type>(seed_int);
  } else {
    cpp11::stop("Invalid type for 'seed'");
  }
  return seed;
}

namespace {

template<typename T>
std::string algorithm_name() {
  std::string ret;
  if (std::is_same<T, xoshiro128plus>::value) {
    ret = "xoshiro128plus";
  } else if (std::is_same<T, xoshiro128plusplus>::value) {
    ret = "xoshiro128plusplus";
  } else if (std::is_same<T, xoshiro128starstar>::value) {
    ret = "xoshiro128starstar";
  } else if (std::is_same<T, xoroshiro128plus>::value) {
    ret = "xoroshiro128plus";
  } else if (std::is_same<T, xoroshiro128plusplus>::value) {
    ret = "xoroshiro128plusplus";
  } else if (std::is_same<T, xoroshiro128starstar>::value) {
    ret = "xoroshiro128starstar";
  } else if (std::is_same<T, xoshiro256plus>::value) {
    ret = "xoshiro256plus";
  } else if (std::is_same<T, xoshiro256plusplus>::value) {
    ret = "xoshiro256plusplus";
  } else if (std::is_same<T, xoshiro256starstar>::value) {
    ret = "xoshiro256starstar";
  } else if (std::is_same<T, xoshiro512plus>::value) {
    ret = "xoshiro512plus";
  } else if (std::is_same<T, xoshiro512plusplus>::value) {
    ret = "xoshiro512plusplus";
  } else if (std::is_same<T, xoshiro512starstar>::value) {
    ret = "xoshiro512starstar";
  }
  return ret;
}

template <typename rng_state_type>
cpp11::raws rng_state_vector(prng<rng_state_type>* rng) {
  auto state = rng->export_state();
  size_t len = sizeof(typename rng_state_type::int_type) * state.size();
  cpp11::writable::raws r_state(len);
  std::memcpy(RAW(r_state), state.data(), len);
  return r_state;
}

}
}
}
}
