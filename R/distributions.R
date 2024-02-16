## Not in base R
rmvnorm <- function(x, vcv) {
  make_rmvnorm(vcv)(x)
}


make_rmvnorm <- function(vcv) {
  n <- ncol(vcv)
  r <- chol(vcv, pivot = TRUE)
  r <- r[, order(attr(r, "pivot", exact = TRUE))]
  function(x) {
    x + drop(rnorm(n) %*% r)
  }
}
