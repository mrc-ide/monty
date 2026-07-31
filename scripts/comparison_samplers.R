# Minimal sampler demo mirroring examples from:

# https://mrc-ide.github.io/odin-monty/samplers.html

# Optional: install and load monty from the nuts-basic branch.

# MONTY_USE_NUTS_BASIC=true Rscript scripts/comparison_samplers.R

use_nuts_basic <- tolower(Sys.getenv("MONTY_USE_NUTS_BASIC", "false")) == "true"

if (use_nuts_basic) {
  
  if (!requireNamespace("remotes", quietly = TRUE)) {
    
    install.packages("remotes", repos = "https://cloud.r-project.org")
    
  }
  
  remotes::install_github("mrc-ide/monty@nuts-basic", upgrade = "never")
  
  library(monty)
  
} else if (requireNamespace("devtools", quietly = TRUE)) {
  
  devtools::load_all(".", quiet = TRUE)
  
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  
  pkgload::load_all(".", quiet = TRUE)
  
} else {
  
  stop("Install 'devtools' or 'pkgload' to run this script from source")
  
}

acceptance_rate <- function(samples) {
  
  pars <- t(drop(samples$pars))
  
  initial <- if (is.matrix(samples$initial)) samples$initial[, 1] else samples$initial
  
  chain <- rbind(initial, pars)
  
  moved <- rowSums(abs(diff(chain)) > 0) > 0
  
  mean(moved)
  
}

lag1_acf <- function(x) {
  
  stats::cor(x[-1], x[-length(x)])
  
}

acf_curve <- function(x, lag_max = 50) {
  
  acf_fit <- stats::acf(x, lag.max = lag_max, plot = FALSE)
  
  list(
    
    lag = as.numeric(acf_fit$lag[, 1, 1]),
    
    acf = as.numeric(acf_fit$acf[, 1, 1])
    
  )
  
}

run_timed_sample <- function(model, sampler, n_steps, seed) {
  
  set.seed(seed)
  
  timing <- system.time({
    
    samples <- monty_sample(model, sampler, n_steps = n_steps)
    
  })
  
  list(samples = samples, elapsed = unname(timing[["elapsed"]]))
  
}

approx_ess <- function(x, lag_max = 50) {
  
  acf_vals <- stats::acf(x, lag.max = lag_max, plot = FALSE)$acf[-1, 1, 1]
  
  pos_acf <- acf_vals[acf_vals > 0]
  
  tau <- 1 + 2 * sum(pos_acf)
  
  length(x) / tau
  
}

build_nuts_sampler <- function(epsilon, max_treedepth, warmup_steps,
                               
                               target_accept = 0.9) {
  
  nuts_formals <- names(formals(monty_sampler_nuts))
  
  args <- list(epsilon = epsilon, max_treedepth = max_treedepth)
  
  if ("warmup_steps" %in% nuts_formals) {
    
    args$warmup_steps <- warmup_steps
    
  }
  
  if ("adapt_step_size" %in% nuts_formals) {
    
    args$adapt_step_size <- TRUE
    
  }
  
  if ("target_accept" %in% nuts_formals) {
    
    args$target_accept <- target_accept
    
  }
  
  do.call(monty_sampler_nuts, args)
  
}

post_burnin <- function(x, burnin_steps) {
  
  if (burnin_steps >= length(x) - 1) {
    
    stop("burnin_steps too large for available samples")
    
  }
  
  x[(burnin_steps + 1):length(x)]
  
}

cat("=== Example 1: Adaptive vs Random-Walk on correlated Gaussian ===\n")

vcv_target <- matrix(c(1, 0.8, 0.8, 1), 2, 2)

m_gaussian <- monty_example("gaussian", vcv_target)

vcv_initial <- diag(2) * 0.1

initial <- c(5, -5)

n_steps <- as.integer(Sys.getenv("MONTY_N_STEPS", "12000"))

if (!is.finite(n_steps) || n_steps < 1000) {
  
  stop("MONTY_N_STEPS must be an integer >= 1000")
  
}

adaptive_end <- min(3000L, max(300L, as.integer(0.3 * n_steps)))

nuts_warmup_steps <- min(4000L, max(500L, as.integer(0.2 * n_steps)))

nuts_target_accept <- as.numeric(Sys.getenv("MONTY_NUTS_TARGET_ACCEPT", "0.9"))

nuts_max_treedepth <- as.integer(Sys.getenv("MONTY_NUTS_MAX_TREEDEPTH", "12"))

burnin_steps <- max(adaptive_end, nuts_warmup_steps)

cat("Using n_steps:", n_steps, "\n")

cat("Adaptive adapt_end:", adaptive_end, "\n")

cat("NUTS warmup_steps:", nuts_warmup_steps, "\n")

cat("NUTS target_accept:", nuts_target_accept, "\n")

cat("NUTS max_treedepth:", nuts_max_treedepth, "\n")

cat("ACF/ESS burn-in used for all samplers:", burnin_steps, "\n")

set.seed(1)

rw <- monty_sample(
  
  m_gaussian,
  
  monty_sampler_random_walk(vcv = vcv_initial),
  
  n_steps = n_steps,
  
  initial = initial
  
)

set.seed(1)

adaptive <- monty_sample(
  
  m_gaussian,
  
  monty_sampler_adaptive(
    
    initial_vcv = vcv_initial,
    
    initial_vcv_weight = 10,
    
    initial_scaling = 1,
    
    acceptance_target = 0.234,
    
    forget_rate = 0.2,
    
    adapt_end = adaptive_end,
    
    boundaries = "reflect"
    
  ),
  
  n_steps = n_steps,
  
  initial = initial
  
)

cat("RW acceptance rate:      ", sprintf("%.3f", acceptance_rate(rw)), "\n")

cat("Adaptive acceptance rate:", sprintf("%.3f", acceptance_rate(adaptive)), "\n")

final_scaling <- {
  
  history <- adaptive$details$scaling_history
  
  if (length(history) > 0) tail(history, 1) else NA_real_
  
}

cat("Adaptive final scaling:  ", sprintf("%.3f", final_scaling), "\n")

cat("Adaptive final VCV:\n")

print(adaptive$details$vcv[, , 1])

cat("\n=== Example 2: NUTS vs HMC vs Adaptive vs Random-Walk on banana model ===\n")

m_banana <- monty_example("banana", sigma = 0.5)

rw_out <- run_timed_sample(
  
  m_banana,
  
  monty_sampler_random_walk(vcv = diag(2) * 0.01),
  
  n_steps = n_steps,
  
  seed = 2
  
)

hmc_out <- run_timed_sample(
  
  m_banana,
  
  monty_sampler_hmc(epsilon = 0.1, n_integration_steps = 10),
  
  n_steps = n_steps,
  
  seed = 2
  
)

adaptive_out <- run_timed_sample(
  
  m_banana,
  
  monty_sampler_adaptive(
    
    initial_vcv = diag(2) * 0.01,
    
    initial_vcv_weight = 10,
    
    initial_scaling = 1,
    
    acceptance_target = 0.234,
    
    forget_rate = 0.2,
    
    adapt_end = adaptive_end,
    
    boundaries = "reflect"
    
  ),
  
  n_steps = n_steps,
  
  seed = 2
  
)

if (!exists("monty_sampler_nuts", mode = "function")) {
  
  stop("NUTS sampler is not available in this build; try MONTY_USE_NUTS_BASIC=true")
  
}

nuts_out <- run_timed_sample(
  
  m_banana,
  
  build_nuts_sampler(
    
    epsilon = 0.1,
    
    max_treedepth = nuts_max_treedepth,
    
    warmup_steps = nuts_warmup_steps,
    
    target_accept = nuts_target_accept
    
  ),
  
  n_steps = n_steps,
  
  seed = 2
  
)

rw_banana <- rw_out$samples

hmc_banana <- hmc_out$samples

adaptive_banana <- adaptive_out$samples

nuts_banana <- nuts_out$samples

rw_alpha <- drop(rw_banana$pars[1, , 1])

hmc_alpha <- drop(hmc_banana$pars[1, , 1])

adaptive_alpha <- drop(adaptive_banana$pars[1, , 1])

nuts_alpha <- drop(nuts_banana$pars[1, , 1])

rw_alpha_eval <- post_burnin(rw_alpha, burnin_steps)

hmc_alpha_eval <- post_burnin(hmc_alpha, burnin_steps)

adaptive_alpha_eval <- post_burnin(adaptive_alpha, burnin_steps)

nuts_alpha_eval <- post_burnin(nuts_alpha, burnin_steps)

cat("RW lag-1 autocorrelation (alpha, post-burnin): ", sprintf("%.3f", lag1_acf(rw_alpha_eval)), "\n")

cat("HMC lag-1 autocorrelation (alpha, post-burnin):", sprintf("%.3f", lag1_acf(hmc_alpha_eval)), "\n")

cat("Adaptive lag-1 autocorrelation (alpha, post-burnin):", sprintf("%.3f", lag1_acf(adaptive_alpha_eval)), "\n")

cat("NUTS lag-1 autocorrelation (alpha, post-burnin):", sprintf("%.3f", lag1_acf(nuts_alpha_eval)), "\n")

cat("RW elapsed seconds:   ", sprintf("%.3f", rw_out$elapsed), "\n")

cat("HMC elapsed seconds:  ", sprintf("%.3f", hmc_out$elapsed), "\n")

cat("Adaptive elapsed seconds:", sprintf("%.3f", adaptive_out$elapsed), "\n")

cat("NUTS elapsed seconds: ", sprintf("%.3f", nuts_out$elapsed), "\n")

if (!is.null(nuts_banana$details$epsilon) && length(nuts_banana$details$epsilon) == 1) {
  
  cat("NUTS final epsilon:   ", sprintf("%.5f", nuts_banana$details$epsilon), "\n")
  
} else {
  
  cat("NUTS final epsilon:    unavailable in sample details\n")
  
}

perf <- data.frame(
  
  sampler = c("Random-Walk", "HMC", "Adaptive", "NUTS"),
  
  lag1_acf_alpha = c(
    
    lag1_acf(rw_alpha_eval),
    
    lag1_acf(hmc_alpha_eval),
    
    lag1_acf(adaptive_alpha_eval),
    
    lag1_acf(nuts_alpha_eval)
    
  ),
  
  elapsed_sec = c(rw_out$elapsed, hmc_out$elapsed, adaptive_out$elapsed, nuts_out$elapsed),
  
  approx_ess_alpha = c(
    
    approx_ess(rw_alpha_eval),
    
    approx_ess(hmc_alpha_eval),
    
    approx_ess(adaptive_alpha_eval),
    
    approx_ess(nuts_alpha_eval)
    
  )
  
)

perf$approx_ess_per_sec <- perf$approx_ess_alpha / perf$elapsed_sec

cat("\nPerformance summary (alpha parameter, post-burnin):\n")

print(within(perf, {
  
  lag1_acf_alpha <- round(lag1_acf_alpha, 3)
  
  elapsed_sec <- round(elapsed_sec, 3)
  
  approx_ess_alpha <- round(approx_ess_alpha, 1)
  
  approx_ess_per_sec <- round(approx_ess_per_sec, 1)
  
}))

lag_max <- 50

hmc_acf <- acf_curve(hmc_alpha_eval, lag_max = lag_max)

adaptive_acf <- acf_curve(adaptive_alpha_eval, lag_max = lag_max)

nuts_acf <- acf_curve(nuts_alpha_eval, lag_max = lag_max)

rw_acf <- acf_curve(rw_alpha_eval, lag_max = lag_max)

graphics::matplot(
  
  rw_acf$lag,
  
  cbind(rw_acf$acf, hmc_acf$acf, adaptive_acf$acf, nuts_acf$acf),
  
  type = "l",
  
  lty = 1,
  
  lwd = 2,
  
  col = c("#d73027", "#1a9850", "#fdae61", "#4575b4"),
  
  xlab = "Lag",
  
  ylab = "ACF",
  
  main = "Banana Model: ACF Comparison (alpha, post-burnin)"
  
)

graphics::abline(h = 0, lty = 3, col = "gray40")

graphics::legend(
  
  "topright",
  
  legend = c("Random-Walk", "HMC", "Adaptive", "NUTS"),
  
  col = c("#d73027", "#1a9850", "#fdae61", "#4575b4"),
  
  lty = 1,
  
  lwd = 2,
  
  bty = "n"
  
)

cat("\nDone. This script reproduces the main sampler comparisons from the docs.\n")
