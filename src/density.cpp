#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>

#include <monty/random/density.hpp>

[[cpp11::register]]
SEXP density_binomial(cpp11::integers x, cpp11::integers size,
                      cpp11::doubles prob, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::binomial<double>(x[i], size[i], prob[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_normal(cpp11::doubles x, cpp11::doubles mu, cpp11::doubles sd,
                    bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::normal<double>(x[i], mu[i], sd[i], log);
  }
  return ret;
}

template <typename T>
SEXP density_negative_binomial_mu_(cpp11::integers x, cpp11::doubles size,
                                   cpp11::doubles mu, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::negative_binomial_mu<T>(x[i], size[i], mu[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_negative_binomial_mu(cpp11::integers x, cpp11::doubles size,
                                  cpp11::doubles mu, bool log, bool is_float) {
  return is_float ?
  density_negative_binomial_mu_<float>(x, size, mu, log) :
  density_negative_binomial_mu_<double>(x, size, mu, log);
}

[[cpp11::register]]
SEXP density_negative_binomial_prob(cpp11::integers x, cpp11::doubles size,
                                    cpp11::doubles prob, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::negative_binomial_prob<double>(x[i], size[i],
                                                            prob[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_beta_binomial_prob(cpp11::integers x, cpp11::integers size,
                                cpp11::doubles prob, cpp11::doubles rho, 
                                bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::beta_binomial_prob<double>(x[i], size[i], prob[i],
                                                        rho[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_beta_binomial_ab(cpp11::integers x, cpp11::integers size,
                              cpp11::doubles a, cpp11::doubles b, 
                              bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::beta_binomial_ab<double>(x[i], size[i], a[i],
                                                      b[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_poisson(cpp11::integers x, cpp11::doubles lambda, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::poisson<double>(x[i], lambda[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_uniform(cpp11::doubles x, cpp11::doubles min, cpp11::doubles max,
                     bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::uniform<double>(x[i], min[i], max[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_exponential_rate(cpp11::doubles x, cpp11::doubles rate, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::exponential_rate<double>(x[i], rate[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_exponential_mean(cpp11::doubles x, cpp11::doubles mean, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::exponential_mean<double>(x[i], mean[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_gamma_rate(cpp11::doubles x, cpp11::doubles shape,
                        cpp11::doubles rate, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::gamma_rate<double>(x[i], shape[i], rate[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_gamma_scale(cpp11::doubles x, cpp11::doubles shape,
                         cpp11::doubles scale, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::gamma_scale<double>(x[i], shape[i], scale[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_beta(cpp11::doubles x, cpp11::doubles a, cpp11::doubles b,
                  bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::beta<double>(x[i], a[i], b[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_log_normal(cpp11::doubles x, cpp11::doubles mulog,
                        cpp11::doubles sdlog, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::log_normal<double>(x[i], mulog[i], sdlog[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_cauchy(cpp11::doubles x, cpp11::doubles location,
                    cpp11::doubles scale, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::cauchy<double>(x[i], location[i], scale[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_weibull(cpp11::doubles x, cpp11::doubles shape,
                     cpp11::doubles scale, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::weibull<double>(x[i], shape[i], scale[i], log);
  }
  return ret;
}

[[cpp11::register]]
SEXP density_zi_poisson(cpp11::integers x, cpp11::doubles pi0,
                        cpp11::doubles lambda, bool log) {
  const size_t n = x.size();
  cpp11::writable::doubles ret(x.size());
  for (size_t i = 0; i < n; ++i) {
    ret[i] = monty::density::zi_poisson<double>(x[i], pi0[i], lambda[i], log);
  }
  return ret;
}
