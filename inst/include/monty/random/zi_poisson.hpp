#pragma once

#include <cmath>

#include "monty/random/generator.hpp"
#include "monty/random/poisson.hpp"

namespace monty {
namespace random {

namespace {

template <typename real_type>
void zi_poisson_validate(real_type pi0, real_type lambda) {
  if (!std::isfinite(pi0) || !std::isfinite(lambda) || pi0 < 0 || pi0 > 1 || lambda < 0) {
    char buffer[256];
    snprintf(buffer, 256,
             "Invalid call to zi_poisson with pi0 = %g, lambda = %g",
             pi0, lambda);
    monty::utils::fatal_error(buffer);
  }
}
}

template <typename real_type, typename rng_state_type>
real_type zi_poisson(rng_state_type& rng_state, real_type pi0, real_type lambda) {
#ifdef __CUDA_ARCH__
  static_assert("zi_poisson() not implemented for GPU targets");
#endif
  zi_poisson_validate(pi0, lambda);
  
  if (rng_state.deterministic) {
    return (1 - pi0) * lambda;
  }
  
  const auto draw_poisson = 
    pi0 == 0 || (pi0 < 1 && random_real<real_type>(rng_state) > pi0);
  
  return draw_poisson ? poisson(rng_state, lambda) : 0;
}

}
}
