{{header}}
#pragma once

#include <cmath>
#include <limits>

#include "monty/random/cuda_compatibility.hpp"

// https://docs.nvidia.com/cuda/cuda-math-api/group__CUDA__MATH__SINGLE.html
// https://stackoverflow.com/a/39409957

namespace monty {
namespace math {

// Automatically generated functions; see scripts/update_monty_math in
// the monty source repo
{{functions}}

// Functions written by hand because they don't generalise usefully

// Special beacuse we nee
template <typename T, typename U>
__host__ __device__
T pow(T x, U y) {
  return std::pow(x, y);
}

#ifdef __CUDA_ARCH__
template <>
__device__
inline float pow(float x, float y) {
  return ::powf(x, y);
}

template <>
__device__
inline float pow(float x, int y) {
  // could possibly use fast power here (see binomial.hpp)
  return ::powf(x, static_cast<float>(y));
}
#endif

// Special because name does not follow pattern:
template <typename T>
__host__ __device__
T abs(T x) {
  return std::abs(x);
}

#ifdef __CUDA_ARCH__
template <>
__device__
inline float abs(float x) {
  return ::fabsf(x);
}
#endif

template <typename T>
__host__ __device__
T min(T a, T b) {
  return a < b ? a : b;
}

template <typename T>
__host__ __device__
T max(T a, T b) {
  return a > b ? a : b;
}

template <typename real_type>
__host__ __device__ real_type lgamma(real_type x) {
  static_assert(std::is_floating_point<real_type>::value,
                "lgamma should only be used with real types");
  return std::lgamma(x);
}

#ifdef __CUDA_ARCH__
template <>
inline __device__ float lgamma(float x) {
  return ::lgammaf(x);
}

template <>
inline __device__ double lgamma(double x) {
  return ::lgamma(x);
}
#endif

template <typename real_type>
__host__ __device__
real_type lfactorial(int x) {
  return lgamma(static_cast<real_type>(x + 1));
}

// We can do this (more efficiently!) with copysign, but end up with
// having a bit of fight with different overloads.  This way is fine.
//
// https://stackoverflow.com/questions/1903954/is-there-a-standard-sign-function-signum-sgn-in-c-c
// https://en.cppreference.com/w/cpp/numeric/math/copysign
template <typename T>
__host__ __device__
T sign(T x) {
  return (T(0) < x) - (x < T(0));
}


inline double fmodr(double x, double y) {
  const auto ret = std::fmod(x, y);
  if (ret * y < 0) {
    ret += y;
  }
  return ret;
}

inline float fmodr(float x, float y) {
  const auto ret = std::fmodf(x, y);
  if (ret * y < 0) {
    ret += y;
  }
  return ret;
}

template <typename real_type>
real_type fintdiv(real_type x, real_type y) {
  return monty::math::floor(x / y);
}

// John D Cook, public domain
template <typename T>
T erf(T x) {
  constexpr auto a1 =  0.254829592;
  constexpr auto a2 = -0.284496736;
  constexpr auto a3 =  1.421413741;
  constexpr auto a4 = -1.453152027;
  constexpr auto a5 =  1.061405429;
  constexpr auto p  =  0.3275911;

  // Save the sign of x
  int sign = x >= 0;
  x = abs(x);

  // A&S formula 7.1.26
  double t = 1.0 / (1.0 + p * x);
  double y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-x * x);

  return sign * y;
}

}
}
