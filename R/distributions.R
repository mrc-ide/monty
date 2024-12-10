## This whole file is probably going to get an overhaul at some point
## because it's quite gross.  Mostly it is trying to tidy up some of
## the ways that we might draw from multivariate normal distributions.
## This is complicated by wanting to cache the results of the vcv
## decomposition where possible.

## This is the form of the Cholesky factorisation of a matrix we use
## in the multivariate normal sampling.
chol_pivot <- function(x) {
  r <- chol(x, pivot = TRUE)
  r[, order(attr(r, "pivot", exact = TRUE))]
}


## There are three uses of this; two use centred and one does not.
##
## * monty_example_gaussian uses centrered
## * sampler_hmc also uses centred
## * make_random_walk_proposal does not use centred, but could


make_rmvnorm <- function(vcv) {
  n <- nrow(vcv)
  if (is.matrix(vcv)) {
    ## Special case for transformations in single-dimensional case; no
    ## need to work with matrix multiplication or decompositions here.
    if (n == 1) {
      sd <- sqrt(drop(vcv))
      function(rng) {
        monty_random_normal(0, sd, rng)
      }
    } else {
      r <- chol_pivot(vcv)
      function(rng) {
        drop(monty_random_n_normal(n, 0, 1, rng) %*% r)
      }
    }
  } else {
    stopifnot(length(dim(vcv)) == 3)
    m <- dim(vcv)[[3]]
    ## At this point we have a vcv that has 'm' entries; we'll be
    ## given a rng that has either 1 stream (PT) or 'm' streams
    ## (simultaneous sampling)
    if (n == 1) {
      r <- sqrt(drop(vcv))
    } else {
      r <- vapply(seq_len(m), function(i) chol_pivot(vcv[, , i]), vcv[, , 1])
    }
    function(rng) {
      n_streams <- length(rng)
      stopifnot(any(n_streams == c(1, m)))
      len <- if (n_streams == 1) n * m else n
      rand <- monty_random_n_normal(len, 0, 1, rng)
      if (n == 1) {
        ret <- drop(rand) * r
      } else {
        ret <- vapply(seq_len(m), function(i) rand[, i] %*% r[, , i],
                      numeric(n))
        if (m == 1 && !attr(rng, "preserve_stream_dimension")) {
          dim(ret) <- NULL
        }
      }
      ret
    }
  }
}


## log density multivariate normal
ldmvnorm <- function(x, vcv) {
  make_ldmvnorm(vcv)(x)
}


make_ldmvnorm <- function(vcv) {
  dec <- base::chol(vcv)
  constant <- -sum(log(diag(dec))) - 0.5 * nrow(vcv) * log(2 * pi)
  function(x) {
    constant - 0.5 * sum(backsolve(dec, x, transpose = TRUE)^2)
  }
}


## Not the world's most beautiful name.
deriv_ldmvnorm <- function(x, vcv) {
  make_deriv_ldmvnorm(vcv)(x)
}


make_deriv_ldmvnorm <- function(vcv) {
  vcv_inv <- solve(vcv)
  function(x) {
    -vcv_inv %*% x
  }
}
