#pragma once

#include <cmath>

#include "monty/random/generator.hpp"
#include "monty/random/poisson.hpp"

namespace monty {
namespace random {

namespace {

template <typename real_type>
void zi_poisson_validate(real_type lambda, real_type pi) {
  if (!std::isfinite(lambda) || lambda < 0 || pi < 0 || pi > 1) {
    char buffer[256];
    snprintf(buffer, 256,
             "Invalid call to zi_poisson with lambda = %g, pi = %g",
             lambda, pi);
    monty::utils::fatal_error(buffer);
  }
}
}

template <typename real_type, typename rng_state_type>
real_type zi_poisson(rng_state_type& rng_state, real_type lambda, real_type pi) {
#ifdef __CUDA_ARCH__
  static_assert("zi_poisson() not implemented for GPU targets");
#endif
  zi_poisson_validate(lambda, pi);
  
  if (rng_state.deterministic) {
    return (1 - pi) * lambda;
  }
  const real_type u = random_real<real_type>(rng_state);
  if (u <= pi) {
    return 0;
  } else {
    return poisson(rng_state, lambda);
  }
  
}

}
}
