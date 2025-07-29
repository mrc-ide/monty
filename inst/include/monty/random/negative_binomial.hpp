#pragma once

#include <cmath>

#include "monty/random/gamma.hpp"
#include "monty/random/poisson.hpp"
#include "monty/random/generator.hpp"

namespace monty {
namespace random {

namespace {

template <typename real_type>
void negative_binomial_validate(real_type size, real_type prob) {
  const bool err = size < 0 || prob < 0 || prob > 1 ||
    (std::isnan(prob) && size > 0) || !std::isfinite(size) || 
    (size > 0 && prob == 0);
  if (err) {
    char buffer[256];
    snprintf(buffer, 256,
             "Invalid call to negative_binomial with size = %g, prob = %g",
             size, prob);
    monty::utils::fatal_error(buffer);
  }
}

}

template <typename real_type, typename rng_state_type>
real_type negative_binomial_prob(rng_state_type& rng_state, real_type size, real_type prob) {
#ifdef __CUDA_ARCH__
  static_assert("negative_binomial_prob() not implemented for GPU targets");
#endif
    negative_binomial_validate(size, prob);

    if (rng_state.deterministic) {
      return size > 0 ? (1 - prob) * size / prob : 0;
    }
    return (size == 0 || prob == 1) ? 0 : poisson(rng_state, gamma_scale(rng_state, size, (1 - prob) / prob));
}

template <typename real_type, typename rng_state_type>
real_type negative_binomial_mu(rng_state_type& rng_state, real_type size, real_type mu) {
#ifdef __CUDA_ARCH__
  static_assert("negative_binomial_mu() not implemented for GPU targets");
#endif
  const auto prob = size / (size + mu);
  negative_binomial_validate(size, prob);
  
  return negative_binomial_prob(rng_state, size, prob);
}

}
}
