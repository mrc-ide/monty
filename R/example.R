##' Load example models from monty.  These models exist so that we can
##' create (hopefully) interesting examples in the documentation
##' without them becoming overwhelming.  You should probably not use
##' these for anything other than exploring the package.
##'
##' Load small, ready-made target distributions for demonstrations, tests,
##' and examples. These models exist so that we can create (hopefully)
##' interesting examples in the documentation without them becoming
##' overwhelming. All examples contain an analytic gradient and can
##' thus be also used to explore gradient-based samplers such as
##' [monty_sampler_hmc].
##'
##' # Supported models:
##'
##' Each model has a **arguments** that are passed through as `...` in
##' `monty_example` and are used to set up the example.  This creates
##' a `monty_model` object that will accept **parameters**, which are
##' the values in the argument `parameters` passed to
##' `monty_model_density`.
##'
##' ## `banana`
##'
##' The banana model is a two-dimensional banana-shaped function, with
##' strong local correlation.  This example was picked because it is
##' useful for illustrating the limitations of random-walk proposals.
##' The model has two parameters `alpha` and `beta` and is based on
##' two successive draws, one conditional on the other.
##'
##' You can vary the argument `sigma` for this model on creation, the
##' default is 0.5
##'
##' ## `gaussian`
##'
##' A multivariate Gaussian centred at the origin.  Takes a
##' variance-covariance-matrix as its argument.  Parameters are
##' letters a, b, ... up to the number of dimensions.
##'
##' ## `ring`
##'
##' A two-dimensional ring-shaped density whose log-density is
##' proportional to $-(\|x\|-r)^2/(2\,sd^2)$.  Includes an analytic
##' gradient and a direct sampler in polar coordinates.  The
##' constructor takes that arguments `r` (default 3) and `sd` (default
##' 0.2), and the parameters of the `monty_model` are `x1` and `x2`
##' (corresponding to the two dimensions of the polar coordinates).
##'
##' Increasing `r` or decreasing `sd` will make the ridge of high
##' probability space narrower; this is easiest to visualise in true
##' densities (rather than log density).
##'
##' ## `mixture2d`
##'
##' An equal-weight mixture of `k` isotropic 2D Gaussians with
##' standard deviation `sd` and centres either provided by the
##' argument (`means`) or drawn uniformly in a square of half-width
##' `spread` (which must be provided if `means` is not
##' provided). Includes an analytic gradient.
##'
##' @param name Name of the example, as a string.  See Details for
##'   supported models.
##'
##' @param ... Optional parameters that are passed to create the
##'   model.  All models can be created with no additional parameters,
##'   but you can tweak their behaviour by passing named parameters
##'   here.  See Details.
##'
##' @return A [monty_model] object.
##'
##' @seealso [monty_model], [monty_model_properties]
##' @export
##'
##' @examples
##' # Small helper function to make the plotting easier
##' show_2d_example <- function(m, rx, ry = rx, n = 101) {
##'   x <- seq(rx[[1]], rx[[2]], length.out = n)
##'   y <- seq(ry[[1]], ry[[2]], length.out = n)
##'   xy <- unname(t(as.matrix(expand.grid(x, y))))
##'   z <- exp(matrix(monty_model_density(m, xy), n, n))
##'   image(x, y, z)
##'   contour(x, y, z, add = TRUE, drawlabels = FALSE,
##'           lwd = 0.5, col = "#00000088")
##' }
##'
##' # Banana target
##' m_banana <- monty_example("banana", sigma = 0.2)
##' show_2d_example(m_banana, c(-0.5, 1.5), c(-1.5, 1.5))
##'
##' # Ring target with radius 3 and narrow thickness
##' m_ring <- monty_example("ring", r = 3, sd = 0.25)
##' show_2d_example(m_ring, c(-4, 4))
##'
##' # 2D Gaussian mixture with 20 components
##' m_mixture <- monty_example("mixture2d", k = 20, sd = 0.5, spread = 4)
##' show_2d_example(m_mixture, c(-5, 5))
monty_example <- function(name, ...) {
  examples <- list(
    banana = monty_example_banana,
    gaussian = monty_example_gaussian,
    ring = monty_example_ring,
    mixture2d = monty_example_gaussian_mixture2d)
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
  assert_scalar_positive_numeric(r, allow_zero = TRUE)
  assert_scalar_positive_numeric(sd, allow_zero = FALSE)

  density <- function(x) {
    if (is.matrix(x)) {
      x1  <- x[1, ]
      x2  <- x[2, ]
    } else {
      x1  <- x[[1]]
      x2  <- x[[2]]
    }
    rho <- sqrt(x1 * x1 + x2 * x2)
    -(rho - r)^2 / (2 * sd * sd)
  }

  gradient <- function(x) {
    if (is.matrix(x)) {
      x1  <- x[1, ]
      x2  <- x[2, ]
      rho <- sqrt(x1 * x1 + x2 * x2)
      rho_safe <- ifelse(rho == 0, 1, rho)
      scale <- -(rho - r) / (sd * sd * rho_safe)
      rbind(scale * x1, scale * x2, deparse.level = 0)
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

  direct_sample <- function(rng) {
    theta <- monty_random_uniform(0, 2 * pi, rng)
    rad <- monty_random_normal(r, sd, rng)
    c(rad * cos(theta), rad * sin(theta))
  }

  properties <- monty_model_properties(allow_multiple_parameters = TRUE)

  monty_model(
    list(parameters = c("x1", "x2"),
         direct_sample = direct_sample,
         density = density,
         gradient = gradient,
         domain = rbind(c(-Inf, Inf), c(-Inf, Inf))),
    properties = properties)
}


monty_example_gaussian_mixture2d <- function(k = 8, sd = 0.4, spread = NULL,
                                             means = NULL) {
  assert_scalar_positive_integer(k, allow_zero = FALSE)
  assert_scalar_positive_numeric(sd, allow_zero = FALSE)
  if (is.null(means)) {
    assert_scalar_positive_numeric(sd, allow_zero = FALSE)
    means <- cbind(runif(k, -spread, spread), runif(k, -spread, spread))
  } else {
    if (!is.null(spread)) {
      cli::cli_abort(
        c("Do not provide 'spread' if providing 'means'",
          i = paste("'spread' is used to randomly sample 'means' if it is",
                    "not given, but you are providing 'means' here already")))
    }
    if (!is.matrx(means)) {
      cli::cli_abort("Expected 'means' to be a matrix")
    }
    if (ncol(means) != 2) {
      cli::cli_abort("Expected 'means' to have two columns")
    }
    if (ncol(means) != 2) {
      cli::cli_abort("Expected 'means' to have {k} row{?s}")
    }
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

  ## TODO: try and harmonise the vector and non-multi-parameter forms here
  density <- function(x) {
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

  gradient <- function(x) {
    inv_var <- 1 / (sd * sd)
    if (is.matrix(x)) {
      gx <- numeric(ncol(x))
      gy <- numeric(ncol(x))
      for (j in seq_len(ncol(x))) {
        xx <- c(x[1, j], x[2, j])
        lp <- log_phi_one(xx)
        log_w_unnorm <- -log(k) + lp
        m  <- max(log_w_unnorm)
        w  <- exp(log_w_unnorm - (m + log(sum(exp(log_w_unnorm - m)))))
        # sum_i w_i (x - mu_i)
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

  direct_sample <- function(rng) {
    i <- ceiling(monty_random_real(k))
    c(monty_random_normal(means[i, 1], sd, rng),
      monty_random_normal(means[i, 2], sd, rng))
  }

  properties <- monty_model_properties(allow_multiple_parameters = TRUE)

  monty_model(
    list(parameters = c("x1", "x2"),
         direct_sample = direct_sample,
         density  = density,
         gradient = gradient,
         domain   = rbind(c(-Inf, Inf), c(-Inf, Inf))),
    properties = properties)
}
