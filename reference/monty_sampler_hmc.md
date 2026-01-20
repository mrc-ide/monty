# Create HMC

Create a Hamiltonian Monte Carlo sampler, implemented using the leapfrog
algorithm.

## Usage

``` r
monty_sampler_hmc(
  epsilon = 0.015,
  n_integration_steps = 10,
  vcv = NULL,
  debug = FALSE
)
```

## Arguments

- epsilon:

  The step size of the HMC steps

- n_integration_steps:

  The number of HMC steps per step

- vcv:

  A variance-covariance matrix for the momentum vector. The default uses
  an identity matrix.

- debug:

  Logical, indicating if we should save all intermediate points and
  their gradients. This will add a vector "history" to the details after
  the integration. This *will* slow things down though as we accumulate
  the history inefficiently.

## Value

A `monty_sampler` object, which can be used with
[monty_sample](https://mrc-ide.github.io/monty/reference/monty_sample.md)
