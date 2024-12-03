set.seed(1) # so we see the same thing
pkgload::load_all()
likelihood <- ex_mixture(5)
prior <- monty_dsl({
  x ~ Normal(0, 10)
})
posterior <- likelihood + prior
s <- monty_sampler_parallel_tempering(n_rungs = 10, vcv = matrix(0.1))
rng1 <- monty_rng$new()
current_state <- s$initialise(2,posterior,rng1)
n_steps <- 2
for(i in 1:n_steps){
  current_state <- s$step(current_state,posterior,rng1)
}

## We "move" the "inflated" state using the RW
internal_state_plus <- environment(s$step)$sampler$step(environment(s$step)$internal$state, environment(s$step)$internal$model, rng1)

## We recover the uncorrected densities from before here
## - note that I don't really understand how $last_density() 
density <- environment(s$step)$internal$model$model$last_density()

beta <- environment(s$step)$beta

## This should be equal to 0's
## element[10] is ~ 0.316 that's big
internal_state_plus$density - beta * density$target - (1 - beta) * density$base


 