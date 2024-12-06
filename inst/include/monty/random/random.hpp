#pragma once

#include "monty/random/generator.hpp"
#include "monty/random/prng.hpp"

#include "monty/random/beta.hpp"
#include "monty/random/beta_binomial.hpp"
#include "monty/random/binomial.hpp"
#include "monty/random/cauchy.hpp"
#include "monty/random/exponential.hpp"
#include "monty/random/gamma.hpp"
#include "monty/random/hypergeometric.hpp"
#include "monty/random/log_normal.hpp"
#include "monty/random/multinomial.hpp"
#include "monty/random/negative_binomial.hpp"
#include "monty/random/truncated_normal.hpp"
#include "monty/random/normal.hpp"
#include "monty/random/poisson.hpp"
#include "monty/random/uniform.hpp"

#include "monty/random/version.hpp"

namespace monty {
namespace random {

namespace {

template <typename T>
struct default_rng_helper;

template <>
struct default_rng_helper<double> {
  using type = xoshiro256plus;
};

template <>
struct default_rng_helper<float> {
  using type = xoshiro128plus;
};

}

template <typename T>
using generator = typename default_rng_helper<T>::type;

}
}
