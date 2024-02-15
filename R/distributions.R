## Not in base R
rmvnorm <- function(x, vcv) {
  n <- ncol(vcv)
  r <- chol(vcv, pivot = TRUE)
  r <- r[, order(attr(r, "pivot", exact = TRUE))]
  x + drop(rnorm(n) %*% r)
}


make_rmvnorm <- function(vcv) {
  n <- ncol(vcv)
  r <- chol(vcv, pivot = TRUE)
  r <- r[, order(attr(r, "pivot", exact = TRUE))]
  function(x) {
    x + drop(rnorm(n) %*% r)
  }
}
