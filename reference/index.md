# Package index

## Models

- [`monty_model()`](https://mrc-ide.github.io/monty/reference/monty_model.md)
  : Create basic model

- [`monty_model_combine()`](https://mrc-ide.github.io/monty/reference/monty_model_combine.md)
  : Combine two models

- [`monty_model_split()`](https://mrc-ide.github.io/monty/reference/monty_model_split.md)
  : Split a combined model

- [`monty_model_function()`](https://mrc-ide.github.io/monty/reference/monty_model_function.md)
  :

  Create `monty_model` from a function computing density

- [`monty_model_properties()`](https://mrc-ide.github.io/monty/reference/monty_model_properties.md)
  : Describe model properties

### Functions for working with models

- [`monty_model_density()`](https://mrc-ide.github.io/monty/reference/monty_model_density.md)
  : Compute log density
- [`monty_model_gradient()`](https://mrc-ide.github.io/monty/reference/monty_model_gradient.md)
  : Compute gradient of log density
- [`monty_model_direct_sample()`](https://mrc-ide.github.io/monty/reference/monty_model_direct_sample.md)
  : Directly sample from a model

### Example

- [`monty_example()`](https://mrc-ide.github.io/monty/reference/monty_example.md)
  : Load example models from monty. These models exist so that we can
  create (hopefully) interesting examples in the documentation without
  them becoming overwhelming. You should probably not use these for
  anything other than exploring the package.

## Domain specific language

- [`monty_dsl()`](https://mrc-ide.github.io/monty/reference/monty_dsl.md)
  : Domain Specific Language for monty
- [`monty_dsl_error_explain()`](https://mrc-ide.github.io/monty/reference/monty_dsl_error_explain.md)
  : Explain monty error
- [`monty_differentiation()`](https://mrc-ide.github.io/monty/reference/monty_differentiation.md)
  : Differentiate expressions

### Advanced

- [`monty_dsl_distributions()`](https://mrc-ide.github.io/monty/reference/monty_dsl_distributions.md)
  : Information about supported distributions
- [`monty_dsl_parse_distribution()`](https://mrc-ide.github.io/monty/reference/monty_dsl_parse_distribution.md)
  : Parse distribution expression

## Draw samples from a model

- [`monty_sample()`](https://mrc-ide.github.io/monty/reference/monty_sample.md)
  : Sample from a model
- [`monty_sample_continue()`](https://mrc-ide.github.io/monty/reference/monty_sample_continue.md)
  : Continue sampling

### Samplers

- [`monty_sampler_random_walk()`](https://mrc-ide.github.io/monty/reference/monty_sampler_random_walk.md)
  : Random Walk Sampler
- [`monty_sampler_adaptive()`](https://mrc-ide.github.io/monty/reference/monty_sampler_adaptive.md)
  : Adaptive Metropolis-Hastings Sampler
- [`monty_sampler_hmc()`](https://mrc-ide.github.io/monty/reference/monty_sampler_hmc.md)
  : Create HMC
- [`monty_sampler_parallel_tempering()`](https://mrc-ide.github.io/monty/reference/monty_sampler_parallel_tempering.md)
  : Parallel Tempering Sampler

### Chain runners

- [`monty_runner_serial()`](https://mrc-ide.github.io/monty/reference/monty_runner_serial.md)
  : Run MCMC chain in series

- [`monty_runner_parallel()`](https://mrc-ide.github.io/monty/reference/monty_runner_parallel.md)
  : Run MCMC chain in parallel

- [`monty_runner_simultaneous()`](https://mrc-ide.github.io/monty/reference/monty_runner_simultaneous.md)
  : Run MCMC chains simultaneously

- [`monty_runner_callr()`](https://mrc-ide.github.io/monty/reference/monty_runner_callr.md)
  :

  Run MCMC chains in parallel with `callr`

### Manually run chains

- [`monty_sample_manual_prepare()`](https://mrc-ide.github.io/monty/reference/monty_sample_manual_prepare.md)
  : Prepare to sample with manual scheduling
- [`monty_sample_manual_run()`](https://mrc-ide.github.io/monty/reference/monty_sample_manual_run.md)
  : Run sample with manual scheduling
- [`monty_sample_manual_info()`](https://mrc-ide.github.io/monty/reference/monty_sample_manual_info.md)
  : Get information about manually scheduled samples
- [`monty_sample_manual_collect()`](https://mrc-ide.github.io/monty/reference/monty_sample_manual_collect.md)
  : Collect manually run samples
- [`monty_sample_manual_cleanup()`](https://mrc-ide.github.io/monty/reference/monty_sample_manual_cleanup.md)
  : Clean up samples
- [`monty_sample_manual_prepare_continue()`](https://mrc-ide.github.io/monty/reference/monty_sample_manual_prepare_continue.md)
  : Prepare to continue sampling with manual scheduling

### Tools

- [`monty_observer()`](https://mrc-ide.github.io/monty/reference/monty_observer.md)
  : Create observer
- [`monty_packer()`](https://mrc-ide.github.io/monty/reference/monty_packer.md)
  : Build a packer
- [`monty_packer_grouped()`](https://mrc-ide.github.io/monty/reference/monty_packer_grouped.md)
  : Build a nested packer
- [`monty_domain_expand()`](https://mrc-ide.github.io/monty/reference/monty_domain_expand.md)
  : Expand (and check) domain against a packer
- [`monty_samples_thin()`](https://mrc-ide.github.io/monty/reference/monty_samples_thin.md)
  : Thin samples
- [`with_trace_random()`](https://mrc-ide.github.io/monty/reference/with_trace_random.md)
  : Trace random number calls

### Advanced

- [`monty_sampler()`](https://mrc-ide.github.io/monty/reference/monty_sampler.md)
  : Create a monty sampler
- [`monty_sampler_properties()`](https://mrc-ide.github.io/monty/reference/monty_sampler_properties.md)
  : Describe sampler properties

## Random numbers

- [`monty_rng_create()`](https://mrc-ide.github.io/monty/reference/monty_rng_create.md)
  : Create a monty random number generator
- [`monty_rng_state()`](https://mrc-ide.github.io/monty/reference/monty_rng_state.md)
  [`monty_rng_set_state()`](https://mrc-ide.github.io/monty/reference/monty_rng_state.md)
  : Get and set random number state
- [`monty_rng_jump()`](https://mrc-ide.github.io/monty/reference/monty_rng_jump.md)
  [`monty_rng_long_jump()`](https://mrc-ide.github.io/monty/reference/monty_rng_jump.md)
  : Jump random number state

- [`monty_random_real()`](https://mrc-ide.github.io/monty/reference/monty_random_real.md)
  [`monty_random_n_real()`](https://mrc-ide.github.io/monty/reference/monty_random_real.md)
  : Sample from Uniform(0, 1)
- [`monty_random_exponential_rate()`](https://mrc-ide.github.io/monty/reference/monty_random_exponential.md)
  [`monty_random_n_exponential_rate()`](https://mrc-ide.github.io/monty/reference/monty_random_exponential.md)
  [`monty_random_exponential_mean()`](https://mrc-ide.github.io/monty/reference/monty_random_exponential.md)
  [`monty_random_n_exponential_mean()`](https://mrc-ide.github.io/monty/reference/monty_random_exponential.md)
  : Sample from exponential distribution
- [`monty_random_poisson()`](https://mrc-ide.github.io/monty/reference/monty_random_poisson.md)
  [`monty_random_n_poisson()`](https://mrc-ide.github.io/monty/reference/monty_random_poisson.md)
  : Sample from Poisson distribution
- [`monty_random_beta()`](https://mrc-ide.github.io/monty/reference/monty_random_beta.md)
  [`monty_random_n_beta()`](https://mrc-ide.github.io/monty/reference/monty_random_beta.md)
  : Sample from beta distribution
- [`monty_random_binomial()`](https://mrc-ide.github.io/monty/reference/monty_random_binomial.md)
  [`monty_random_n_binomial()`](https://mrc-ide.github.io/monty/reference/monty_random_binomial.md)
  : Sample from binomial distribution
- [`monty_random_cauchy()`](https://mrc-ide.github.io/monty/reference/monty_random_cauchy.md)
  [`monty_random_n_cauchy()`](https://mrc-ide.github.io/monty/reference/monty_random_cauchy.md)
  : Sample from Cauchy distribution
- [`monty_random_gamma_scale()`](https://mrc-ide.github.io/monty/reference/monty_random_gamma.md)
  [`monty_random_n_gamma_scale()`](https://mrc-ide.github.io/monty/reference/monty_random_gamma.md)
  [`monty_random_gamma_rate()`](https://mrc-ide.github.io/monty/reference/monty_random_gamma.md)
  [`monty_random_n_gamma_rate()`](https://mrc-ide.github.io/monty/reference/monty_random_gamma.md)
  : Sample from a gamma distribution. There are two parameterisations
  here, one in terms of rate, and one in terms of scale.
- [`monty_random_negative_binomial_prob()`](https://mrc-ide.github.io/monty/reference/monty_random_negative_binomial.md)
  [`monty_random_n_negative_binomial_prob()`](https://mrc-ide.github.io/monty/reference/monty_random_negative_binomial.md)
  [`monty_random_negative_binomial_mu()`](https://mrc-ide.github.io/monty/reference/monty_random_negative_binomial.md)
  [`monty_random_n_negative_binomial_mu()`](https://mrc-ide.github.io/monty/reference/monty_random_negative_binomial.md)
  : Sample from negative binomial distribution
- [`monty_random_normal()`](https://mrc-ide.github.io/monty/reference/monty_random_normal.md)
  [`monty_random_n_normal()`](https://mrc-ide.github.io/monty/reference/monty_random_normal.md)
  : Sample from normal distribution
- [`monty_random_uniform()`](https://mrc-ide.github.io/monty/reference/monty_random_uniform.md)
  [`monty_random_n_uniform()`](https://mrc-ide.github.io/monty/reference/monty_random_uniform.md)
  : Sample from uniform distribution
- [`monty_random_beta_binomial_prob()`](https://mrc-ide.github.io/monty/reference/monty_random_beta_binomial.md)
  [`monty_random_n_beta_binomial_prob()`](https://mrc-ide.github.io/monty/reference/monty_random_beta_binomial.md)
  [`monty_random_beta_binomial_ab()`](https://mrc-ide.github.io/monty/reference/monty_random_beta_binomial.md)
  [`monty_random_n_beta_binomial_ab()`](https://mrc-ide.github.io/monty/reference/monty_random_beta_binomial.md)
  : Sample from beta-binomial distribution
- [`monty_random_hypergeometric()`](https://mrc-ide.github.io/monty/reference/monty_random_hypergeometric.md)
  [`monty_random_n_hypergeometric()`](https://mrc-ide.github.io/monty/reference/monty_random_hypergeometric.md)
  : Sample from hypergeometric distribution
- [`monty_random_truncated_normal()`](https://mrc-ide.github.io/monty/reference/monty_random_truncated_normal.md)
  [`monty_random_n_truncated_normal()`](https://mrc-ide.github.io/monty/reference/monty_random_truncated_normal.md)
  : Sample from truncated normal
- [`monty_random_log_normal()`](https://mrc-ide.github.io/monty/reference/monty_random_log_normal.md)
  [`monty_random_n_log_normal()`](https://mrc-ide.github.io/monty/reference/monty_random_log_normal.md)
  : Sample from log-normal
- [`monty_random_weibull()`](https://mrc-ide.github.io/monty/reference/monty_random_weibull.md)
  [`monty_random_n_weibull()`](https://mrc-ide.github.io/monty/reference/monty_random_weibull.md)
  : Sample from Weibull
- [`monty_random_zi_poisson()`](https://mrc-ide.github.io/monty/reference/monty_random_zi_poisson.md)
  [`monty_random_n_zi_poisson()`](https://mrc-ide.github.io/monty/reference/monty_random_zi_poisson.md)
  : Sample from zero-inflated Poisson distribution
- [`monty_random_zi_negative_binomial_prob()`](https://mrc-ide.github.io/monty/reference/monty_random_zi_negative_binomial.md)
  [`monty_random_n_zi_negative_binomial_prob()`](https://mrc-ide.github.io/monty/reference/monty_random_zi_negative_binomial.md)
  [`monty_random_zi_negative_binomial_mu()`](https://mrc-ide.github.io/monty/reference/monty_random_zi_negative_binomial.md)
  [`monty_random_n_zi_negative_binomial_mu()`](https://mrc-ide.github.io/monty/reference/monty_random_zi_negative_binomial.md)
  : Sample from zero-inflated negative binomial distribution
