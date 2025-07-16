#pragma once

#include <cmath>

#include "monty/random/gamma.hpp"
#include "monty/random/negative_binomial.hpp"
#include "monty/random/generator.hpp"

namespace monty {
namespace random {

namespace {

template <typename real_type>
void zi_negative_binomial_validate(real_type size, real_type prob, real_type pi) {
  if (!std::isfinite(size) || !std::isfinite(prob) || size <= 0 || prob <= 0 || prob > 1 || pi < 0  || pi > 1) {
    char buffer[256];
    snprintf(buffer, 256,
             "Invalid call to zi_negative_binomial with size = %g, prob = %g, pi = %g",
             size, prob, pi);
    monty::utils::fatal_error(buffer);
  }
}

}

template <typename real_type, typename rng_state_type>
real_type zi_negative_binomial_prob(rng_state_type& rng_state, real_type size, real_type prob, real_type pi) {
#ifdef __CUDA_ARCH__
  static_assert("negative_binomial_prob() not implemented for GPU targets");
#endif
    zi_negative_binomial_validate(size, prob, pi);

    if (rng_state.deterministic) {
      return (1 - pi) * (1 - prob) * size / prob;
    }
    
    const real_type u = random_real<real_type>(rng_state);
    if (u <= pi) {
      return 0;
    } else {
      return negative_binomial_prob(rng_state, size, prob);
    }
}

template <typename real_type, typename rng_state_type>
real_type zi_negative_binomial_mu(rng_state_type& rng_state, real_type size, real_type mu, real_type pi) {
#ifdef __CUDA_ARCH__
  static_assert("negative_binomial_mu() not implemented for GPU targets");
#endif
  const auto prob = size / (size + mu);
  zi_negative_binomial_validate(size, prob, pi);
  
  return zi_negative_binomial_prob(rng_state, size, prob, pi);
}

}
}
