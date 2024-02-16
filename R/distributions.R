## Not in base R
rmvnorm <- function(x, vcv, rng) {
  make_rmvnorm(vcv)(x, rng)
}


make_rmvnorm <- function(vcv, centred = FALSE) {
  n <- ncol(vcv)
  r <- chol(vcv, pivot = TRUE)
  r <- r[, order(attr(r, "pivot", exact = TRUE))]
  if (centred) {
    function(rng) {
      drop(rng$random_normal(n) %*% r)
    }
  } else {
    function(x, rng) {
      x + drop(rng$random_normal(n) %*% r)
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
