#pragma once

#include <climits>
#include <cmath>

#include "monty/random/binomial_gamma_tables.hpp"
#include "monty/random/generator.hpp"
#include "monty/random/math.hpp"

namespace monty {
namespace random {

// Faster version of pow(x, n) for integer 'n' by using
// "exponentiation by squaring"
// https://en.wikipedia.org/wiki/Exponentiation_by_squaring
template <typename real_type, typename int_type>
__host__ __device__
real_type fast_pow(real_type x, int_type n) {
  real_type pow = 1.0;
  if (n != 0) {
    while (true) {
      if(n & 01) {
        pow *= x;
      }
      if(n >>= 1) {
        x *= x;
      } else {
        break;
      }
    }
  }
  return pow;
}

__nv_exec_check_disable__
template <typename real_type, typename int_type>
__host__ __device__
real_type binomial_inversion_calc(real_type u, int_type n, real_type p) {
  const real_type q = 1 - p;
  const real_type r = p / q;
  const real_type g = r * (n + 1);
  real_type f = fast_pow(q, n);
  int_type k = 0;

  // We hit this branch when n * p has expectation of 10 or less
  // (e.g., p = 0.5, n = 5 would be ok but p = 0.5, n = 30 would go
  // through the BTRS route).
  //
  // As n increases the only way that np can be < 10 is if we have
  // most of the mass in the small samples; as p decreases it squashes
  // more up the y axis, and even with very large n it is never very
  // high.
  //
  // Consider the probability of a sample of size 'm' or greater; this is
  //
  //   pbinom(m, n, p, FALSE)
  //
  // Because of the setup above, this function is monotonically
  // increasing with p, so consider the case where np = 10 as a worst case
  //
  //   pbinom(m, n, 10 / n, FALSE)
  //
  // In the R sources they use a value 'm' of 110 (with their
  // inclusion level for inversion of np < 30)
  //
  //   plot(sapply(10^(2:10), function(n) pbinom(110, n, 10 / n, FALSE)))
  //
  // This does monotonically incraese with n, but remains really very
  // small, rising to an error of ~1e-30 in the R case:
  //
  //   pbinom(110, 1e10, 30 / 1e10, FALSE)
  //
  // The equivalent cuttoff for us would be 63:
  //
  //   qbinom(6.6e-30, 1e10, 10 / 1e10, FALSE)
  const int_type max_k = std::min(n, static_cast<int_type>(63));
  while (u >= f) {
    u -= f;
    k++;
    f *= (g / k - r);
    if (k > max_k) {
      return -1;
    }
  }

  return k;
}

// Binomial random numbers via inversion (for low np only!). Draw a
// random number from U(0, 1) and find the 'n' up the distribution
// (given p) that corresponds to this
__nv_exec_check_disable__
template <typename real_type, typename int_type, typename rng_state_type>
__host__ __device__
real_type binomial_inversion(rng_state_type& rng_state, int_type n, real_type p) {
  real_type k = -1;
  do {
    real_type u = random_real<real_type>(rng_state);
    k = binomial_inversion_calc(u, n, p);
  } while (k < 0);
  return k;
}

template <typename real_type>
__host__ __device__ real_type stirling_approx_tail(real_type k);

template <typename real_type>
__host__ __device__ inline real_type stirling_approx_tail_calc(real_type k) {
  const real_type one = 1;
  real_type kp1sq = (k + 1) * (k + 1);
  return (one / 12 - (one / 360 - one / 1260 / kp1sq) / kp1sq) / (k + 1);
}

template <>
__host__ __device__ inline float stirling_approx_tail(float k) {
  float tail;
  if (k <= k_tail_values_max_f) {
    tail = k_tail_values_f[static_cast<int>(k)];
  } else {
    tail = stirling_approx_tail_calc(k); // #nocov
  }
  return tail;
}

template <>
__host__ __device__ inline double stirling_approx_tail(double k) {
  double tail;
  if (k <= k_tail_values_max_d) {
    tail = k_tail_values_d[static_cast<int>(k)];
  } else {
    tail = stirling_approx_tail_calc(k);
  }
  return tail;
}

// https://www.tandfonline.com/doi/abs/10.1080/00949659308811496
__nv_exec_check_disable__
template <typename real_type, typename rng_state_type>
inline __host__ __device__
real_type btrs(rng_state_type& rng_state, real_type n, real_type p) {
  const real_type one = 1.0;
  const real_type half = 0.5;

  // This is spq in the paper.
  const real_type stddev = monty::math::sqrt(n * p * (1 - p));

  // Other coefficients for Transformed Rejection sampling.
  const real_type b = static_cast<real_type>(1.15) + static_cast<real_type>(2.53) * stddev;
  const real_type a = static_cast<real_type>(-0.0873) + static_cast<real_type>(0.0248) * b + static_cast<real_type>(0.01) * p;
  const real_type c = n * p + half;
  const real_type v_r = static_cast<real_type>(0.92) - static_cast<real_type>(4.2) / b;
  const real_type r = p / (1 - p);

  const real_type alpha = (static_cast<real_type>(2.83) +
                           static_cast<real_type>(5.1) / b) * stddev;
  const real_type m = std::floor((n + 1) * p);

  real_type draw;
  while (true) {
    real_type u = random_real<real_type>(rng_state);
    real_type v = random_real<real_type>(rng_state);
    u -= half;
    real_type us = half - monty::math::abs(u);
    real_type k = std::floor((2 * a / us + b) * u + c);

    // Region for which the box is tight, and we
    // can return our calculated value This should happen
    // 0.86 * v_r times. In the limit as n * p is large,
    // the acceptance rate converges to ~79% (and in the lower
    // regime it is ~24%).
    if (us >= static_cast<real_type>(0.07) && v <= v_r) {
      draw = k;
      break;
    }
    // Reject non-sensical answers.
    if (k < 0 || k > n) {
      continue;
    }

    // This deviates from Hormann's BRTS algorithm, as there is a log missing.
    // For all (u, v) pairs outside of the bounding box, this calculates the
    // transformed-reject ratio.
    v = monty::math::log(v * alpha / (a / (us * us) + b));
    real_type upperbound =
      ((m + half) * monty::math::log((m + 1) / (r * (n - m + 1))) +
       (n + one) * monty::math::log((n - m + 1) / (n - k + 1)) +
       (k + half) * monty::math::log(r * (n - k + 1) / (k + 1)) +
       stirling_approx_tail(m) + stirling_approx_tail(n - m) -
       stirling_approx_tail(k) - stirling_approx_tail(n - k));
    if (v <= upperbound) {
      draw = k;
      break;
    }
  }
  return draw;
}

template <typename real_type>
__host__ __device__
void binomial_validate(real_type n, real_type p) {
  const bool err = n < 0 || p < 0 || p > 1 ||
    (std::isnan(p) && n > 0) ||
    (std::isnan(n) && p > 0);
  if (err) {
    char buffer[256];
    snprintf(buffer, 256,
             "Invalid call to binomial with n = %.0f, p = %g, q = %g",
             n, p, 1 - p);
    monty::utils::fatal_error(buffer);
  }
}

template <typename real_type>
__host__ real_type binomial_deterministic(real_type n, real_type p) {
  if (n < 0) {
    if (n * n < std::numeric_limits<real_type>::epsilon()) {
      // Avoid small round-off errors here
      n = std::round(n);
    } else {
      char buffer[256];
      snprintf(buffer, 256, "Invalid call to binomial with n = %f", n);
      throw std::runtime_error(buffer);
    }
  }
  binomial_validate(n, p);
  return n > 0 && p > 0 ? n * p : 0;
}

// NOTE: we return a real, not an int, as with deterministic mode this
// will not necessarily be an integer. This helps though in cases
// where n is greater than INT_MAX
template <typename real_type, typename rng_state_type>
__host__ __device__
real_type binomial_stochastic(rng_state_type& rng_state, real_type n,
                              real_type p) {
  binomial_validate(n, p);
  real_type draw;

  if (n == 0 || p == 0) {
    draw = 0;
  } else if (p == 1) {
    draw = n;
  } else {
    real_type q = p;
    if (p > static_cast<real_type>(0.5)) {
      q = 1 - q;
    }

    if (n * q >= 10) {
      draw = btrs(rng_state, n, q);
    } else if (n < INT_MAX) {
      draw = binomial_inversion(rng_state, static_cast<int>(n), q);
    } else {
      draw = binomial_inversion(rng_state, static_cast<size_t>(n), q);
    }

    if (p > static_cast<real_type>(0.5)) {
      draw = n - draw;
    }
  }

  SYNCWARP
  return draw;
}

/// Draw a binomially distributed random number; the number of
/// successes given `n` trials each with probability `p`. Generation
/// is performed using a rejection-sampling algorithm or inversion
/// depending on the expected mean (`n * p`).
///
/// @tparam real_type The underlying real number type, typically
/// `double` or `float`. A compile-time error will be thrown if you
/// attempt to use a non-floating point type (based on
/// `std::is_floating_point).
///
/// @tparam rng_state_type The random number state type
///
/// @param rng_state Reference to the random number state, will be
/// modified as a side-effect
///
/// @param n The number of trials
///
/// @param p The probability of success of each trial
template <typename real_type, typename rng_state_type>
__host__ __device__
real_type binomial(rng_state_type& rng_state, real_type n, real_type p) {
  static_assert(std::is_floating_point<real_type>::value,
                "Only valid for floating-point types; use binomial<real_type>()");
#ifndef __CUDA_ARCH__
  if (rng_state.deterministic) {
    return binomial_deterministic<real_type>(n, p);
  }
#endif
  // Avoid integer truncation (which a cast to int would cause) in
  // case of numerical error, instead taking the slightly lower but
  // more accurate round route. This means that `n - eps` becomes
  // `n` not `n - 1`. Also avoids truncation if n > INT_MAX
  return binomial_stochastic<real_type>(rng_state, std::round(n), p);
}

}
}
