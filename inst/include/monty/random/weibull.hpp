#pragma once

#include <cmath>

#include "monty/random/generator.hpp"
#include "monty/random/numeric.hpp"
#include "monty/random/math.hpp"

namespace monty {
namespace random {

namespace {

template <typename real_type>
void weibull_validate(real_type shape, real_type scale) {
  if (shape < 0.0 || scale < 0.0) {
    char buffer[256];
    snprintf(buffer, 256,
             "Invalid call to Weibull with shape = %g, scale = %g",
             shape, scale);
    monty::utils::fatal_error(buffer);
  }
}
}

template <typename real_type, typename rng_state_type>
real_type weibull(rng_state_type& rng_state, real_type shape, real_type scale) {
#ifdef __CUDA_ARCH__
  static_assert("weibull() not implemented for GPU targets");
#endif
  weibull_validate(shape, scale);
  
  if (rng_state.deterministic) {
    return scale * std::tgamma(1 + 1 / shape);
  }
  return scale * (-monty::math::log(1 - random_real<real_type>(rng_state))) ^ (1 / shape);
}

}
}
