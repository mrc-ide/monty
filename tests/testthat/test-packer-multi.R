
## monty_packer2(
##   scalar,
##   fixed,
##   groups = ...,
##   shared = c("a", "b"),
##   fixed = 

p <- monty_packer_grouped(
  groups = c("a", "b", "c"),
  scalar = c("x", "y"),
  shared = "y")
p$unpack(c(1, 2, 3, 4))

## Pack in this order:
## y (shared)
## x{a}, x{b}, x{c}

## Unpack into
## a = list(x = x{a}, y = y)
## b = list(x = x{b}, y = y)
## c = list(x = x{c}, y = y)

x <- 10
m <- monty_dsl({
  mu <- c(...)
  beta[] ~ Normal(mu[i], 1)
})
m$density(2)
