#Create a bi-modal double Gaussian model
mixture_gaussians <- function(symetric_means = 5) {
  mcstate_model(list(
    parameters = c("x"),
    direct_sample = function(rng) {
      rng$random_normal(1)
    },
    density = function(x) {
      log(.5 * dnorm(x+symetric_means) +  .5* dnorm(x-symetric_means))
    },
    gradient = function(x){
     -((x+symetric_means)*dnorm(x+symetric_means)+(x-symetric_means)*dnorm(x-symetric_means))/(dnorm(x+symetric_means) +  dnorm(x-symetric_means)) 
    },
    domain = cbind(rep(-Inf, 1), rep(Inf, 1))))
}

m <- mixture_gaussians(10)

#Create an HMC sampler
sampler <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)
#sampler <- mcstate_sampler_random_walk(vcv = matrix(0.1))

#Sample m using HMC sampler
set.seed(1)
res <- mcstate_sample(m, sampler, 200000)
hist(res$pars, breaks=100)

# 
# ls(sampler)
# ls(environment(sampler$initialise))
# 
# sampler$initialise(m$direct_sample(rng),m,rng)


#Create a RNG
rng <- mcstate_rng$new()

#Create an initial state for a PT sampler using m
x_0 <- list()
x_0$pars <- m$direct_sample(rng)
x_0$density <- m$density(x_0$pars)

#Create internal$transform and internal$sample_momentum
sampler$initialise(m$direct_sample(rng),m,rng)

#Do one HMC step
sampler$step(x_0,m,rng)

x<-x_0
pars_res <- x$pars
for(i in 1:10000)
{
  x <- sampler$step(x,m,rng)
  pars_res <- rbind(pars_res, x$pars)
}
plot(pars_res[,1], pars_res[,2])

m1 <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)
m2 <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)

bb <- list(m1,m2)
bb

N_temp <- 15
beta <- seq(0,N_temp)/N_temp
PT_samplers <- lapply(beta,FUN = function(x) {
  samp <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)
  samp$beta <- x
  samp
  })
