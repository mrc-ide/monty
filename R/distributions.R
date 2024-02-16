## Not in base R
rmvnorm <- function(x, vcv) {
  make_rmvnorm(vcv)(x)
}


##' @importFrom stats rnorm
make_rmvnorm <- function(vcv) {
  n <- ncol(vcv)
  r <- chol(vcv, pivot = TRUE)
  r <- r[, order(attr(r, "pivot", exact = TRUE))]
  function(x) {
    x + drop(rnorm(n) %*% r)
  }
}


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
dldmvnormdx <- function(x, vcv) {
  make_dldmvnormdx(vcv)(x)
}


make_dldmvnormdx <- function(vcv) {
  vcv_inv <- solve(vcv)
  function(x) {
    -vcv_inv %*% x
  }
}
