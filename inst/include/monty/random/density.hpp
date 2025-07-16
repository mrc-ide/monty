#pragma once

#include <cmath>
#include <limits>
#include <type_traits>

#include "monty/random/cuda_compatibility.hpp"
#include "monty/random/numeric.hpp"
#include "monty/random/math.hpp"

namespace monty {
namespace density {

namespace {

CONSTANT double m_ln_sqrt_2pi_dbl = 0.918938533204672741780329736406;
CONSTANT float m_ln_sqrt_2pi_flt = 0.918938533204672741780329736406f;

// Returns m_ln_sqrt_2pi
template <typename real_type>
__host__ __device__ real_type norm_integral();

template<>
__host__ __device__ inline double norm_integral() {
  return m_ln_sqrt_2pi_dbl;
}

template<>
__host__ __device__ inline float norm_integral() {
  return m_ln_sqrt_2pi_flt;
}

__nv_exec_check_disable__
template <typename T>
__host__ __device__ T maybe_log(T x, bool log) {
  return log ? x : monty::math::exp(x);
}

template <typename T>
__host__ __device__ T lchoose(T n, T k) {
  return random::utils::lgamma(static_cast<T>(n + 1)) -
    random::utils::lgamma(static_cast<T>(k + 1)) -
    random::utils::lgamma(static_cast<T>(n - k + 1));
}

template <typename T>
__host__ __device__ T lbeta(T x, T y) {
  return random::utils::lgamma(x) + random::utils::lgamma(y) -
    random::utils::lgamma(x + y);
}

}

template <typename T>
__host__ __device__ T binomial(T x, T size, T prob, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "binomial should only be used with real types");
#endif
  T ret;
  if (x == 0 && size == 0) {
    ret = 0;
  } else {
    ret = lchoose<T>(size, x) +
      x * monty::math::log(prob) +
      (size - x) * monty::math::log(1 - prob);
  }

  SYNCWARP
  return maybe_log(ret, log);
}

template <typename T>
__host__ __device__ T dirac_delta(T x, bool log) {
  const T inf = random::utils::infinity<T>();
  return maybe_log(x == 0 ? inf : -inf, log);
}

template <typename T>
__host__ __device__ T normal(T x, T mu, T sd, bool log) {
  T ret;
  if (sd == 0) {
    ret = dirac_delta(x - mu, log); // This does maybe_log
  } else {
    const T dx = x - mu;
    ret = - dx * dx / (2 * sd * sd) - norm_integral<T>() - monty::math::log(sd);
    ret = maybe_log(ret, log);
  }

  SYNCWARP
  return ret;
}

template <typename T>
__host__ __device__ T negative_binomial_mu(T x, T size, T mu, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "negative_binomial should only be used with real types");
#endif
  T ret;
  if (x == 0 && size == 0) {
    ret = 0;
  }
  else if (x < 0 || size == 0) {
    ret = -random::utils::infinity<T>();
  }
  else if (mu == 0) {
    ret = x == 0 ? 0 : -random::utils::infinity<T>();
  } else {
    // Avoid size / (size + mu) when size is close to zero, and this
    // would cause prob to be equal to zero. Somewhat arbitrarily,
    // taking 100 * floating point eps as the change over.
    const T ratio = random::utils::epsilon<T>() * 100;
    if (mu < ratio * size) {
      const T log_prob = monty::math::log(mu / (1 + mu / size));
      ret = x * log_prob - mu - random::utils::lgamma(x + 1) +
        monty::math::log1p(x * (x - 1) / (2 * size));
    } else {
      const T prob = size / (size + mu);
      ret = random::utils::lgamma(x + size) -
        random::utils::lgamma(size) -
        random::utils::lgamma(x + 1) +
        size * monty::math::log(prob) + x * monty::math::log(1 - prob);
    }
  }

  SYNCWARP
  return maybe_log(ret, log);
}

// This may not be stable for all size and prob, but provides
// compatibility with R's C-level function. See ?dnbinom for details.
template <typename T>
__host__ __device__ T negative_binomial_prob(T x, T size, T prob, bool log) {
  const T mu = size * (1 - prob) / prob;
  return negative_binomial_mu(x, size, mu, log);
}

template <typename T>
__host__ __device__ T beta_binomial_ab(T x, T size, T a, T b, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "beta_binomial should only be used with real types");
#endif
  T ret;
  if (x == 0 && size == 0) {
    ret = 0;
  } else {
    ret = lchoose<T>(size, x) + lbeta(x + a, size - x + b) - lbeta(a, b);
  }
  
  SYNCWARP
  return maybe_log(ret, log);
}


// A note on this parametrisation:
//
//   prob = a / (a + b)
//   rho = 1 / (a + b + 1)
//
// Where a and b have (0, Inf) support
template <typename T>
__host__ __device__ T beta_binomial_prob(T x, T size, T prob, T rho,
                                         bool log) {
  const T a = prob * (1 / rho - 1);
  const T b = (1 - prob) * (1 / rho - 1);
  return beta_binomial_ab(x, size, a, b, log);
}

template <typename T>
__host__ __device__ T poisson(T x, T lambda, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "poisson should only be used with real types");
#endif
  T ret;
  if (x == 0 && lambda == 0) {
    ret = 0;
  } else {
    ret = x * monty::math::log(lambda) - lambda -
      random::utils::lgamma(x + 1);
  }

  SYNCWARP
  return maybe_log(ret, log);
}

template <typename T>
__host__ __device__ T exponential_rate(T x, T rate, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "exponential should only be used with real types");
#endif
  return maybe_log(monty::math::log(rate) - rate * x, log);
}

// TODO: rename as scale
template <typename T>
__host__ __device__ T exponential_mean(T x, T mean, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "exponential should only be used with real types");
#endif
  return maybe_log(-monty::math::log(mean) - x / mean, log);
}

template <typename T>
__host__ __device__ T gamma_rate(T x, T shape, T rate, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "gamma should only be used with real types");
#endif
  const auto ret = (shape - 1) * monty::math::log(x) - rate * x -
    random::utils::lgamma(shape) + shape * monty::math::log(rate);
  return maybe_log(ret, log);
}

template <typename T>
__host__ __device__ T gamma_scale(T x, T shape, T scale, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "gamma should only be used with real types");
#endif
  const auto ret = (shape - 1) * monty::math::log(x) - x / scale -
    random::utils::lgamma(shape) - shape * monty::math::log(scale);
  return maybe_log(ret, log);
}


template <typename T>
__host__ __device__ T uniform(T x, T min, T max, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "uniform should only be used with real types");
#endif
  const auto ret = (x < min || x > max) ? 0 : 1 / (max - min);
  return log ? monty::math::log(ret) : ret;
}

template <typename T>
__host__ __device__ T hypergeometric(T x, T n1, T n2, T k, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "hypergeometric should only be used with real types");
#endif
  const auto ret = lchoose<T>(n1, x) + lchoose<T>(n2, k - x) -
    lchoose<T>(n1 + n2, k);
  return maybe_log(ret, log);
}

template <typename T>
__host__ __device__ T beta(T x, T a, T b, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "beta should only be used with real types");
#endif
  const auto ret = (a - 1) * monty::math::log(x) +
    (b - 1) * monty::math::log(1 - x) - lbeta(a, b);
  return maybe_log(ret, log);
}

template <typename T>
__host__ __device__ T truncated_normal(T x, T mu, T sd, T min, T max, bool log) {
  const auto d = normal(x, mu, sd);

  const auto z_min = 0.5 * (1 + std::erf(min / monty::math::sqrt(2)));
  const auto z_max = 0.5 * (1 + std::erf(max / monty::math::sqrt(2)));
  const auto z = z_max - z_min;

  return log ? (d - monty::math::log(z)) : (monty::math::exp(d) / z);
}

template <typename T>
__host__ __device__ T cauchy(T x, T location, T scale, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "cauchy should only be used with real types");
#endif
  const auto ret = -monty::math::log(M_PI) - monty::math::log(scale) - 
    monty::math::log1p(monty::math::pow((x - location) / scale, 2));
  return maybe_log(ret, log);
}

template <typename T>
__host__ __device__ T weibull(T x, T shape, T scale, bool log) {
#ifndef __CUDA_ARCH__
  static_assert(std::is_floating_point<T>::value,
                "weibull should only be used with real types");
#endif
  const auto ret = monty::math::log(shape) + (shape - 1) * monty::math::log(x) -
    shape * monty::math::log(scale) - (x / scale)^shape;
  return maybe_log(ret, log);
}

template <typename T>
__host__ __device__ T log_normal(T x, T mulog, T sdlog, bool log) {
  const auto d = normal(monty::math::log(x), mulog, sdlog);
  
  return log ? (d - monty::math::log(x)) : (monty::math::exp(d) / x);
}

template <typename T>
__host__ __device__ T zi_poisson(T x, T lambda, T pi, bool log) {
  const auto d = poisson(x, lambda);
  
  T ret;
  if (x == 0) {
    ret = monty::math::log(pi + (1 - pi) * monty::math::exp(d));
  } else {
    ret = monty::math::log(1 - pi) + d;
  }
  
  return maybe_log(ret, log);
}

template <typename T>
__host__ __device__ T zi_negative_binomial_prob(T x, T size, T prob, T pi,
                                                bool log) {
  const auto d = negative_binomial_prob(x, size, prob);
  
  T ret;
  if (x == 0) {
    ret = monty::math::log(pi + (1 - pi) * monty::math::exp(d));
  } else {
    ret = monty::math::log(1 - pi) + d;
  }
  
  return maybe_log(ret, log);
}

template <typename T>
__host__ __device__ T zi_negative_binomial_mu(T x, T size, T mu, T pi,
                                              bool log) {
  const auto d = negative_binomial_mu(x, size, mu);
  
  T ret;
  if (x == 0) {
    ret = monty::math::log(pi + (1 - pi) * monty::math::exp(d));
  } else {
    ret = monty::math::log(1 - pi) + d;
  }
  
  return maybe_log(ret, log);
}

}
}
