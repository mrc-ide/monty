#pragma once

#include <cmath>

#include "monty/random/gamma.hpp"
#include "monty/random/negative_binomial.hpp"
#include "monty/random/generator.hpp"

namespace monty {
namespace random {

namespace {

template <typename real_type>
void zi_negative_binomial_validate(real_type pi0, real_type size, real_type prob) {
  const bool err = pi0 < 0 || pi0 > 1 || size < 0 || prob < 0 || prob > 1 ||
    !std::isfinite(pi0) ||
    (pi0 < 1 && std::isnan(prob) && size > 0) ||
    (pi0 < 1 && !std::isfinite(size)) || 
    (pi0 < 1 && size > 0 && prob == 0);
  if (err) {
    char buffer[256];
    snprintf(buffer, 256,
             "Invalid call to zi_negative_binomial with pi0 = %g, size = %g, prob = %g",
             pi0, size, prob);
    monty::utils::fatal_error(buffer);
  }
}

}

template <typename real_type, typename rng_state_type>
real_type zi_negative_binomial_prob(rng_state_type& rng_state, real_type pi0, real_type size, real_type prob) {
#ifdef __CUDA_ARCH__
  static_assert("negative_binomial_prob() not implemented for GPU targets");
#endif
    zi_negative_binomial_validate(pi0, size, prob);

    if (rng_state.deterministic) {
      return (pi0 < 1 && size > 0) ? (1 - pi0) * (1 - prob) * size / prob : 0;
    }
    
    const auto draw_negative_binomial = 
      pi0 == 0 || (pi0 < 1 && random_real<real_type>(rng_state) > pi0);
    
    return draw_negative_binomial ? negative_binomial_prob(rng_state, size, prob) : 0;
}

template <typename real_type, typename rng_state_type>
real_type zi_negative_binomial_mu(rng_state_type& rng_state, real_type pi0, real_type size, real_type mu) {
#ifdef __CUDA_ARCH__
  static_assert("negative_binomial_mu() not implemented for GPU targets");
#endif
  const auto prob = size / (size + mu);
  zi_negative_binomial_validate(pi0, size, prob);
  
  return zi_negative_binomial_prob(rng_state, pi0, size, prob);
}

}
}
