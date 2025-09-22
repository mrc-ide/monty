#' Example models for \pkg{monty}
#'
#' Load small, ready-made target distributions for demonstrations, tests,
#' and examples. These models exist so that we can create (hopefully)
#' interesting examples in the documentation without them becoming
#' overwhelming. All examples contain an analytic gradient and can
#' thus be also used to explore gradient-based samplers such as
#' \link{monty_sampler_hmc}.
#'
#' @section Supported models:
#' \describe{
#'   \item{\code{"banana"}}{A two-dimensional “banana”-shaped target with
#'   strong local correlation; useful for illustrating the limitations of
#'   random-walk proposals. Tunable by \code{sigma}.}
#'
#'   \item{\code{"gaussian"}}{A centred multivariate Gaussian \eqn{N(0, \Sigma)}
#'   with user-supplied variance–covariance matrix \eqn{\Sigma}.}
#'
#'   \item{\code{"ring"}}{A two-dimensional ring-shaped density whose
#'   log-density is proportional to \eqn{-(\|x\|-r)^2/(2\,sd^2)}. Includes an
#'   analytic gradient and a direct sampler in polar coordinates.}
#'
#'   \item{\code{"mixture2d"}}{An equal-weight mixture of \eqn{k} isotropic
#'   2D Gaussians with standard deviation \code{sd} and centres either
#'   provided by the user (\code{means}) or drawn uniformly in a square of
#'   half-width \code{spread}. Includes an analytic gradient.}
#' }
#'
#' @param name String giving the model name. One of
#'   \code{"banana"}, \code{"gaussian"}, \code{"ring"}, \code{"mixture2d"}.
#' @param ... Additional arguments forwarded to the specific model
#'   constructor (see sections below for each model’s parameters).
#'
#' @return A \link{monty_model} object with fields \code{parameters},
#'   \code{density} (log-density), \code{gradient}, \code{direct_sample}
#'   (where available), and \code{domain}. All returned models set
#'   \code{properties = monty_model_properties(allow_multiple_parameters = TRUE)}
#'   so they can be evaluated for parameter matrices.
#'
#' @details
#' These models are primarily intended for examples, unit tests, and
#' teaching materials. They aim to be simple but non‑trivial, and they
#' expose analytic gradients when that’s helpful for HMC/NUTS.
#'
#' @examples
#' # Banana target
#' m1 <- monty_example("banana")                   # default sigma = 0.5
#' m2 <- monty_example("banana", sigma = 0.2)
#'
#' # 2D Gaussian with diagonal covariance
#' m3 <- monty_example("gaussian", diag(2))
#'
#' # Ring target with radius 3 and narrow thickness
#' m4 <- monty_example("ring", r = 3, sd = 0.25)
#'
#' # 2D Gaussian mixture with 12 components
#' m5 <- monty_example("mixture2d", k = 12, sd = 0.35, spread = 4, seed = 42)
#'
#' @seealso \link{monty_model}, \link{monty_model_properties}
#' @export
monty_example <- function(name, ...) {
  examples <- list(
    banana      = monty_example_banana,
    gaussian    = monty_example_gaussian,
    ring        = monty_example_ring,
    mixture2d   = monty_example_gaussian_mixture2d
  )
  examples[[match_value(name, names(examples))]](...)
}


monty_example_banana <- function(sigma = 0.5) {
  monty_model(
    list(
      parameters = c("alpha", "beta"),
      direct_sample = function(rng) {
        beta <- monty_random_normal(0, 1, rng)
        alpha <- monty_random_normal(beta^2, sigma, rng)
        if (length(alpha) == 1) {
          c(alpha, beta)
        } else {
          rbind(alpha, beta)
        }
      },
      density = function(x) {
        if (is.matrix(x)) {
          alpha <- x[1, ]
          beta <- x[2, ]
        } else {
          alpha <- x[[1]]
          beta <- x[[2]]
        }
        stats::dnorm(beta, log = TRUE) +
          stats::dnorm((alpha - beta^2) / sigma, log = TRUE)
      },
      gradient = function(x) {
        if (is.matrix(x)) {
          alpha <- x[1, ]
          beta <- x[2, ]
        } else {
          alpha <- x[[1]]
          beta <- x[[2]]
        }
        da <- (beta^2 - alpha) / sigma^2
        db <- -beta + 2 * beta * (alpha - beta^2) / sigma^2
        if (is.matrix(x)) {
          rbind(da, db, deparse.level = 0)
        } else {
          c(da, db)
        }
      },
      domain = rbind(c(-Inf, Inf), c(-Inf, Inf))),
    properties = monty_model_properties(allow_multiple_parameters = TRUE))
}


monty_example_gaussian <- function(vcv) {
  n <- nrow(vcv)
  monty_model(list(
    parameters = letters[seq_len(n)],
    direct_sample = make_rmvnorm(vcv),
    density = make_ldmvnorm(vcv),
    gradient = make_deriv_ldmvnorm(vcv),
    domain = cbind(rep(-Inf, n), rep(Inf, n))))
}


monty_example_ring <- function(r = 3, sd = 0.2) {
  stopifnot(sd > 0, r >= 0)
  
  ring_log_density <- function(x) {
    if (is.matrix(x)) {
      x1  <- x[1, ]
      x2  <- x[2, ]
      rho <- sqrt(x1 * x1 + x2 * x2)
      -(rho - r)^2 / (2 * sd * sd)
    } else {
      x1  <- x[[1]]
      x2  <- x[[2]]
      rho <- sqrt(x1 * x1 + x2 * x2)
      -(rho - r)^2 / (2 * sd * sd)
    }
  }
  
  ring_gradient <- function(x) {
    if (is.matrix(x)) {
      x1  <- x[1, ]
      x2  <- x[2, ]
      rho <- sqrt(x1 * x1 + x2 * x2)
      # handle rho = 0 gracefully
      rho_safe <- ifelse(rho == 0, 1, rho)
      scale <- -(rho - r) / (sd * sd * rho_safe)
      gx <- scale * x1
      gy <- scale * x2
      rbind(gx, gy, deparse.level = 0)
    } else {
      x1  <- x[[1]]
      x2  <- x[[2]]
      rho <- sqrt(x1 * x1 + x2 * x2)
      if (rho == 0) {
        c(0, 0)
      } else {
        scale <- -(rho - r) / (sd * sd * rho)
        c(scale * x1, scale * x2)
      }
    }
  }
  
  monty_model(
    list(
      parameters = c("x1", "x2"),
      direct_sample = function(rng) {
        theta <- runif(1, 0, 2 * pi)
        rad   <- monty_random_normal(r, sd, rng)
        c(rad * cos(theta), rad * sin(theta))
      },
      density  = ring_log_density,  # log-density up to a constant
      gradient = ring_gradient,
      domain   = rbind(c(-Inf, Inf), c(-Inf, Inf))
    ),
    properties = monty_model_properties(allow_multiple_parameters = TRUE)
  )
}


monty_example_gaussian_mixture2d <- function(k = 8, sd = 0.4, spread = 3,
                                             means = NULL, seed = NULL) {
  stopifnot(k >= 1, sd > 0, spread > 0)
  if (!is.null(seed)) set.seed(seed)
  if (is.null(means)) {
    means <- cbind(runif(k, -spread, spread), runif(k, -spread, spread))
  } else {
    means <- as.matrix(means)
    stopifnot(ncol(means) == 2, nrow(means) == k)
  }
  
  log_const <- -log(2 * pi * sd * sd)
  
  ## Compute log N_2(x | mu_i, sd^2 I) for a x
  log_phi_one <- function(x) {
    dx <- x[1] - means[, 1]
    dy <- x[2] - means[, 2]
    log_const - 0.5 * (dx * dx + dy * dy) / (sd * sd)
  }
  
  ## Numerically stable log-sum-exp
  lse <- function(v) {
    m <- max(v)
    m + log(sum(exp(v - m)))
  }
  
  density_fun <- function(x) {
    if (is.matrix(x)) {
      out <- numeric(ncol(x))
      for (j in seq_len(ncol(x))) {
        lp <- log_phi_one(c(x[1, j], x[2, j]))
        out[j] <- -log(k) + lse(lp)  # log( (1/k)*sum phi_i )
      }
      out
    } else {
      lp <- log_phi_one(c(x[[1]], x[[2]]))
      -log(k) + lse(lp)
    }
  }
  
  gradient_fun <- function(x) {
    inv_var <- 1 / (sd * sd)
    if (is.matrix(x)) {
      gx <- numeric(ncol(x))
      gy <- numeric(ncol(x))
      for (j in seq_len(ncol(x))) {
        xx <- c(x[1, j], x[2, j])
        lp <- log_phi_one(xx)            # length k
        log_w_unnorm <- -log(k) + lp     # equal weights
        m  <- max(log_w_unnorm)
        w  <- exp(log_w_unnorm - (m + log(sum(exp(log_w_unnorm - m)))))  # normalised
        # ∑_i w_i (x - mu_i)
        mx <- sum(w * (xx[1] - means[, 1]))
        my <- sum(w * (xx[2] - means[, 2]))
        gx[j] <- -inv_var * mx
        gy[j] <- -inv_var * my
      }
      rbind(gx, gy, deparse.level = 0)
    } else {
      xx <- c(x[[1]], x[[2]])
      lp <- log_phi_one(xx)
      log_w_unnorm <- -log(k) + lp
      m  <- max(log_w_unnorm)
      w  <- exp(log_w_unnorm - (m + log(sum(exp(log_w_unnorm - m)))))
      mx <- sum(w * (xx[1] - means[, 1]))
      my <- sum(w * (xx[2] - means[, 2]))
      -inv_var * c(mx, my)
    }
  }
  
  monty_model(
    list(
      parameters = c("x1", "x2"),
      direct_sample = function(rng) {
        i <- sample.int(k, 1)
        c(
          monty_random_normal(means[i, 1], sd, rng),
          monty_random_normal(means[i, 2], sd, rng)
        )
      },
      density  = density_fun,   # proper log-density (mixture is normalised)
      gradient = gradient_fun,
      domain   = rbind(c(-Inf, Inf), c(-Inf, Inf))
    ),
    properties = monty_model_properties(allow_multiple_parameters = TRUE)
  )
}
