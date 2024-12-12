#include <vector>
#include <string>
#include <sstream>

#include <cpp11.hpp>


#include <monty/random/generator.hpp>
#include <monty/random/prng.hpp>

template <typename T>
std::string to_string(const T& t) {
  std::ostringstream ss;
  ss << t;
  return ss.str();
}

[[cpp11::register]]
std::vector<std::string> test_xoshiro_run(cpp11::sexp ptr) {
  using default_rng64 = monty::random::prng<monty::random::xoshiro256plus>;
  auto rng = cpp11::as_cpp<cpp11::external_pointer<default_rng64>>(ptr).get();
  auto& state = rng->state(0);

  constexpr int n = 10;

  std::vector<std::string> ret;
  for (int i = 0; i < 3 * n; ++i) {
    if (i == n - 1) {
      monty::random::jump(state);
    } else if (i == 2 * n - 1) {
      monty::random::long_jump(state);
    }
    auto x = monty::random::next(state);
    ret.push_back(to_string(x));
  }

  return ret;
}
