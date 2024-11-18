#pragma once

#include <stdexcept>

#include "monty/random/density.hpp"
#include "monty/random/generator.hpp"
#include "monty/random/normal.hpp"
#include "monty/random/numeric.hpp"
#include "monty/random/uniform.hpp"
#include "monty/random/math.hpp"

// Method of Robert, from https://arxiv.org/pdf/0907.4010
//
// Other, possibly more efficient, methods are available, but this one
// is pretty easy to implement.

namespace monty {
namespace random {

template <typename real_type, typename rng_state_type>
real_type truncated_normal_standard_2_sided(rng_state_type& rng_state, real_type min, real_type max) {
  real_type z, p_accept;
  while (true) {
    z = uniform<real_type>(rng_state, min, max);
    const auto u = random_real<real_type>(rng_state);
    if (min > 0) {
      p_accept = monty::math::exp((min * max - z * z) / 2);
    } else if (max < 0) {
      p_accept = monty::math::exp((max * max - z * z) / 2);
    } else {
      p_accept = monty::math::exp(-z * z / 2);
    }
    if (u < p_accept) {
      break;
    }
  }
  return z;
}

template <typename real_type, typename rng_state_type>
real_type truncated_normal_standard_1_sided(rng_state_type& rng_state, real_type min) {
  real_type z;
  while (true) {
    const auto a_star = (min + monty::math::sqrt(min * min + 4)) / 2;
    z = monty::random::exponential_rate<real_type>(rng_state, a_star) + min;
    const auto u = random_real<real_type>(rng_state);
    const auto p_accept = monty::math::exp(-(z - a_star) * (z - a_star) / 2);
    if (u < p_accept) {
      break;
    }
  }
  return z;
}

template <typename real_type>
real_type truncated_normal_standard_mean(real_type min, real_type max) {
  constexpr real_type m_1_sqrt_2 = 0.70710678118654746172; // 1 / sqrt(2)
  const auto z_min = 0.5 * (1 + std::erf(min * m_1_sqrt_2));
  const auto z_max = 0.5 * (1 + std::erf(max * m_1_sqrt_2));
  const auto d_min = density::normal<real_type>(min, 0, 1, false);
  const auto d_max = density::normal<real_type>(max, 0, 1, false);
  return (d_min - d_max) / (z_max - z_min);
}

template <typename real_type, typename rng_state_type>
real_type truncated_normal_standard(rng_state_type& rng_state, real_type min, real_type max) {

  const bool min_infinite = min == -std::numeric_limits<real_type>::infinity();
  const bool max_infinite = max == std::numeric_limits<real_type>::infinity();

  if (min_infinite && max_infinite) {
    return random_normal<real_type>(rng_state);
  }

  if (rng_state.deterministic) {
    return truncated_normal_standard_mean(min, max);
  }

  if (max_infinite) {
    return truncated_normal_standard_1_sided(rng_state, min);
  } if (min_infinite) {
    return -truncated_normal_standard_1_sided(rng_state, -max);
  } else {
    return truncated_normal_standard_2_sided(rng_state, min, max);
  }
}


template <typename real_type, typename rng_state_type>
real_type truncated_normal(rng_state_type& rng_state, real_type mean, real_type sd, real_type min, real_type max) {
  const auto a = (min - mean) / sd;
  const auto b = (max - mean) / sd;
  const auto z = truncated_normal_standard(rng_state, a, b);
  return z * sd + mean;
}

}
}
