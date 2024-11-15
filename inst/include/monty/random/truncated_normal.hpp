#pragma once

#include <stdexcept>

#include "monty/random/generator.hpp"
#include "monty/random/normal.hpp"
#include "monty/random/numeric.hpp"
#include "monty/random/uniform.hpp"
#include "monty/random/math.hpp"

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
real_type truncated_normal_1_sided(rng_state_type& rng_state, real_type min) {
  real_type z, p_accept;
  while (true) {
    const auto a_star = (min + monty::math::sqrt(min * min + 4)) / 2;
    z = monty::random::exponential_rate<real_type>(rng_state, a_star) + min;
    const auto u = random_real<real_type>(rng_state);
    if (u < p_accept) {
      break;
    }
  }
  return z;
}

template <typename real_type, typename rng_state_type>
real_type truncated_normal_standard(rng_state_type& rng_state, real_type min, real_type max) {
  if (rng_state.deterministic) {
    // We need to work with the normalising constant here, itself
    // involving erf.  Not that hard, just not done yet.
    throw std::runtime_error("Deterministic truncated normal not yet supported");
  }

  const bool min_infinite = min == -std::numeric_limits<real_type>::infinity;
  const bool max_infinite = max == std::numeric_limits<real_type>::infinity;

  if (min_infinite && max_infinite) {
    return truncated_normal_standard_2_sided(rng_state, min, max);
  } else if (max_infinite) {
    return truncated_normal_standard_1_sided(rng_state, min);
  } if (min_infinite) {
    return -truncated_normal_standard_1_sided(rng_state, -max);
  } else {
    return random_normal<real_type>(rng_state);
  }
}

template <typename real_type, typename rng_state_type>
real_type truncated_normal(rng_state_type& rng_state, real_type mean, real_type sd, real_type min, real_type max) {
  const auto z = truncated_normal_standard(rng_state, min, max);
  return z * sd + mean;
}

}
}
