#pragma once

#include <cmath>
#include <vector>

#include "monty/random/gamma.hpp"
#include "monty/random/generator.hpp"
#include "monty/random/numeric.hpp"

namespace monty {
namespace random {

/// Draw one sample from the Dirichlet distribution.
///
/// @tparam real_type The underlying real number type, typically
/// `double` or `float`. A compile-time error will be thrown if you
/// attempt to use a non-floating point type (based on
/// `std::is_floating_point).
///
/// @tparam rng_state_type The random number state type
///
/// @tparam T,U The type of the containers for `alpha` and `ret`. This
/// might be `double*` or `std::vector<double>` depending on use.
///
/// @param rng_state Reference to the random number state, will be
/// modified as a side-effect
///
/// @param alpha  The set of shape parameters
///
/// @param alpha_len The number of shape parameters (or outcomes)
///
/// @param ret Container for the return value
template <typename real_type, typename rng_state_type,
          typename T, typename U>
__host__ __device__
void dirichlet(rng_state_type& rng_state, const T& alpha,
                 int alpha_len, U ret) {
  real_type alpha_tot = 0;
  for (int i = 0; i < alpha_len; ++i) {
    if (alpha[i] < 0) {
      monty::utils::fatal_error("Negative alpha passed to dirichlet");
    }
    alpha_tot += alpha[i];
  }
  if (alpha_tot == 0) {
    monty::utils::fatal_error("No positive alpha in call to dirichlet");
  }

  real_type ret_tot = 0;
  for (int i = 0; i < alpha_len; ++i) {
    if (alpha[i] > 0) {
      ret[i] = gamma_scale<real_type>(rng_state, alpha[i], 1);
    } else {
      ret[i] = 0;
    }
    ret_tot += ret[i];
  }
  
  for (int i = 0; i < alpha_len; ++i) {
    ret[i] = ret[i] / ret_tot;
  }
}

// These ones are designed for us within standalone programs and won't
// actually be tested by default which is not great.
template <typename real_type, typename rng_state_type>
void dirichlet(rng_state_type& rng_state,
               const std::vector<real_type>& alpha,
               std::vector<real_type>& ret) {
  dirichlet<real_type>(rng_state, alpha, alpha.size(), ret);
}

template <typename real_type, typename rng_state_type>
std::vector<real_type> dirichlet(rng_state_type& rng_state,
                                 const std::vector<real_type>& alpha) {
  std::vector<real_type> ret(alpha.size());
  dirichlet(rng_state, alpha, ret);
  return ret;
}

}
}
