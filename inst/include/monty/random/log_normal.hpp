#pragma once

#include <cmath>

#include "monty/random/generator.hpp"
#include "monty/random/normal.hpp"

namespace monty {
namespace random {

namespace {

template <typename real_type>
void log_normal_validate(real_type meanlog, real_type sdlog) {
  if (!std::isfinite(meanlog) || !std::isfinite(sdlog) || sdlog <= 0) {
    char buffer[256];
    snprintf(buffer, 256,
             "Invalid call to log_normal with meanlog = %g, sdlog = %g",
             meanlog, sdlog);
    monty::utils::fatal_error(buffer);
  }
}

}

template <typename real_type, typename rng_state_type>
real_type log_normal(rng_state_type& rng_state, real_type meanlog, real_type sdlog) {
#ifdef __CUDA_ARCH__
  static_assert("log_normal() not implemented for GPU targets");
#endif
    log_normal_validate(meanlog, sdlog);

    if (rng_state.deterministic) {
      return monty::math::exp(meanlog + monty::math::pow(sdlog, 2) / 2);
    }
    return monty::math::exp(normal(rng_state, meanlog, sdlog));
}

}
}
