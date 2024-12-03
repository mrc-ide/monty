set.seed(1) # so we see the same thing
pkgload::load_all()
likelihood <- ex_mixture(5)
prior <- monty_dsl({
  x ~ Normal(0, 10)
})
posterior <- likelihood + prior

PT_model <- parallel_tempering_scale(posterior, prior, beta)

s <- monty_sampler_parallel_tempering(n_rungs = 10, vcv = matrix(0.1))
res <- monty_sample(posterior, s, 10000, n_chains = 4)

hist(c(res$pars), freq = FALSE)
x <- seq(min(res$pars), max(res$pars), length.out = 1001)
y <- exp(posterior$density(rbind(x)))
lines(x, y / sum(y) / diff(x)[[1]], col = "red")



set.seed(1) # so we see the same thing
pkgload::load_all()
likelihood <- ex_mixture(5)
prior <- monty_dsl({
  x ~ Normal(0, 10)
})
posterior <- likelihood + prior
s <- monty_sampler_parallel_tempering(n_rungs = 10, vcv = matrix(0.1))
rng1 <- monty_rng$new()
n_steps <- 2
res <- rep(0,n_steps)
current_state <- s$initialise(2,posterior,rng1)
for(i in 1:n_steps){
  current_state <- s$step(current_state,posterior,rng1)
  # a <- environment(s$step)$internal$state$density
  # b <- posterior$density(environment(s$step)$internal$state$pars)*beta+(1-beta)*prior$density(environment(s$step)$internal$state$pars)
  # print(setequal(a,b))
  res[i] <- current_state$pars
}



#This should be the same and it is not always
a <- environment(s$step)$internal$state$density
b <- posterior$density(environment(s$step)$internal$state$pars)*beta+(1-beta)*prior$density(environment(s$step)$internal$state$pars)

