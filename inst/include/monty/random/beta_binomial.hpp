#pragma once

#include <cmath>

#include "monty/random/beta.hpp"
#include "monty/random/binomial.hpp"
#include "monty/random/generator.hpp"

namespace monty {
namespace random {

namespace {

template <typename real_type>
void beta_binomial_validate(real_type size, real_type a, real_type b) {
   if(!R_FINITE(size) || !R_FINITE(a) || !R_FINITE(b) || size <= 0 || a <= 0 || b <= 0) {
    char buffer[256];
    snprintf(buffer, 256,
             "Invalid call to beta_binomial with size = %g, a = %g, b = %g", 
             size, a, b);
    monty::utils::fatal_error(buffer);
  }
}

}

template <typename real_type, typename rng_state_type>
real_type beta_binomial_ab(rng_state_type& rng_state, real_type size, real_type a, real_type b) {
#ifdef __CUDA_ARCH__
  static_assert("beta_binomial_ab() not implemented for GPU targets");
#endif
    beta_binomial_validate(size, a, b);

    if (rng_state.deterministic) {
      return size * a / (a + b);
    }
    return binomial(rng_state, size, beta(rng_state, a, b));
}

template <typename real_type, typename rng_state_type>
real_type beta_binomial_prob(rng_state_type& rng_state, real_type size, real_type prob, real_type rho) {
#ifdef __CUDA_ARCH__
  static_assert("beta_binomial_prob() not implemented for GPU targets");
#endif
  const auto a = prob * (1 / rho - 1);
  const auto b = (1 - prob) * (1 / rho - 1);
  
  return beta_binomial_ab(rng_state, size, a, b);
}

}
}
