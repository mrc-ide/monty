test_that("can reflect parameters", {
  expect_equal(reflect_proposal(0.4, 0, 1), 0.4)
  expect_equal(reflect_proposal(1.4, 0, 1), 0.6)
  expect_equal(reflect_proposal(2.4, 0, 1), 0.4)

  expect_equal(reflect_proposal(6, 1, 5), 4)
  expect_equal(reflect_proposal(0, 1, 5), 2)
  expect_equal(reflect_proposal(10, 1, 5), 2)

  expect_equal(reflect_proposal(0:2 + 0.4, rep(0, 3), rep(1, 3)),
               c(0.4, 0.6, 0.4))
  expect_equal(reflect_proposal(0:2 + 1.4, rep(1, 3), rep(2, 3)),
               c(1.4, 1.6, 1.4))
})


test_that("Reflect parameters: infinite", {
  p <- rnorm(10)
  expect_equal(reflect_proposal(p, rep(-Inf, 5), rep(Inf, 5)), p)
})


test_that("reflect parameters: lower", {
  expect_equal(reflect_proposal(-0.4, 0, Inf), 0.4)
  expect_equal(reflect_proposal(0.6, 1, Inf), 1.4)
  expect_equal(reflect_proposal(0.6, 0, Inf), 0.6)
})


test_that("reflect parameters: upper", {
  expect_equal(reflect_proposal(1.4, -Inf, 1), 0.6)
  expect_equal(reflect_proposal(0.4, -Inf, 0), -0.4)
  expect_equal(reflect_proposal(0.4, -Inf, 1), 0.4)
})


test_that("can reflect parameters in matrix form", {
  p <- matrix(1:12, 3, 4) + .1

  expect_equal(reflect_proposal(p, rep(-Inf, 3), rep(Inf, 3)), p)

  x_min <- c(3, -Inf, 9)
  x_max <- c(5, Inf, 10)
  expect_equal(reflect_proposal(p, x_min, x_max),
               sapply(1:4, function(i) reflect_proposal(p[, i], x_min, x_max)))

  xa <- c(3, 4, 9)
  xb <- c(Inf, Inf, Inf)
  expect_equal(reflect_proposal(p, xa, xb),
               sapply(1:4, function(i) reflect_proposal(p[, i], xa, xb)))
  expect_equal(reflect_proposal(p, -xb, xa),
               sapply(1:4, function(i) reflect_proposal(p[, i], -xb, xa)))


  mvn <- make_rmvnorm(diag(3) * c(4, 8, 16))
  r <- monty_rng_create(seed = 42)
  x <- replicate(10, mvn(r))
  x_min <- c(-2, -Inf, -1)
  x_max <- c(2, Inf, 1)
  expect_equal(reflect_proposal(x, x_min, x_max),
               apply(x, 2, reflect_proposal, x_min, x_max))
})


test_that("Can create a reflected random walk proposal", {
  vcv <- diag(3) * c(4, 8, 16)
  domain <- cbind(c(-2, -Inf, -1), c(2, Inf, 1))
  x <- rep(0, 3)

  p1 <- make_random_walk_proposal_fn(vcv, domain, "ignore")
  p2 <- make_random_walk_proposal_fn(vcv, domain[c(2, 2, 2), ], "reflect")
  p3 <- make_random_walk_proposal_fn(vcv, domain, "reflect")

  expect_equal(body(p2), body(p1))

  r1 <- monty_rng_create(seed = 42)
  r2 <- monty_rng_create(seed = 42)
  r3 <- monty_rng_create(seed = 42)

  x1 <- replicate(10, p1(x, r1))
  x2 <- replicate(10, p2(x, r2))
  x3 <- replicate(10, p3(x, r3))

  expect_identical(x1, x2)
  expect_equal(x3, reflect_proposal(x1, domain[, 1], domain[, 2]))
})
