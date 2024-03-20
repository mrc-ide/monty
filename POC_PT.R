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

#tempered mixture model
#beta = 0, density is dnorm(0,1) - our prior density
#beta = 1, density is dnorm(0,1)*pdf(mixture) - can be seen as our posterior
tempered_mixture <- function(symetric_means = 5, beta) {
  mcstate_model(list(
    parameters = c("x"),
    direct_sample = function(rng) { #still direct sample from Normal(0,1)
      rng$random_normal(1)
    },
    density = function(x) {
      #Note that here we assume some sort of Bayesian case with beta = 0 is prior and beta = 1, prior*mixture_gaussian
      #Note also that the density is unormalised!
      beta*log(.5 * dnorm(x+symetric_means) +  .5* dnorm(x-symetric_means))+dnorm(x, log=TRUE)
    },
    gradient = function(x){
      beta*(-((x+symetric_means)*dnorm(x+symetric_means)+(x-symetric_means)*dnorm(x-symetric_means))/(dnorm(x+symetric_means) +  dnorm(x-symetric_means)))-x 
    },
    domain = cbind(rep(-Inf, 1), rep(Inf, 1))))
}

#plot(the posterior density)
m_T <- tempered_mixture(10, 1)
x <- seq(-15,15,length.out=1000)
plot(x, exp(m_T$density(x)), type="l")

#Create an HMC sampler
sampler <- mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10)
#sampler <- mcstate_sampler_random_walk(vcv = matrix(0.1))

#Sample m using HMC sampler
set.seed(1)
res <- mcstate_sample(m_T, sampler, 20) #200000)
hist(res$pars, breaks=100)

N_temp <- 15
beta <- seq(0,N_temp)/N_temp
beta_index <- seq(N_temp+1)
#Create N_temp+1 "machines" to ease the leap to parallel implementation
#In a serial version we could just use the same sampler
PT_machine <- lapply(beta,FUN = function(x) {list(
  sampler = mcstate_sampler_hmc(epsilon = 0.1, n_integration_steps = 10),
  beta = x,
  model = tempered_mixture(10, x),
  rng = mcstate_rng$new())
  })

#initialisation of the "machines"
for(i in seq(N_temp+1)){
  PT_machine[[i]]$state$pars <- PT_machine[[i]]$model$direct_sample(PT_machine[[i]]$rng)
  PT_machine[[i]]$state$density <- PT_machine[[i]]$model$density(PT_machine[[i]]$state$pars)
  PT_machine[[i]]$sampler$initialise(PT_machine[[i]]$model$direct_sample(PT_machine[[i]]$rng),PT_machine[[i]]$model,PT_machine[[i]]$rng)
}

m <- mixture_gaussians(10) #the likelihood model (target over prior)
even_step <- FALSE
n_iterations <- 10000
x_res <- NULL
beta_res <- PT_machine[[N_temp+1]]$beta
index_res <- beta_index
for(k in seq(n_iterations)){
  
  #all the machine do 1 HMC step => exploration step
  for(i in seq(N_temp+1)){
    if(PT_machine[[i]]$beta==0)
    {
      PT_machine[[i]]$state$pars <- PT_machine[[i]]$model$direct_sample(PT_machine[[i]]$rng)
      PT_machine[[i]]$state$density <- PT_machine[[i]]$model$density(PT_machine[[i]]$state$pars)
    } else {
      PT_machine[[i]]$state <- PT_machine[[i]]$sampler$step(PT_machine[[i]]$state,
                                                            PT_machine[[i]]$model,
                                                            PT_machine[[i]]$rng)
    }}
  
  #communication step
  if(even_step){
    index <- which(seq(N_temp)%%2==0)
    } else {
      index <- which(seq(N_temp)%%2==1) 
    }
  
  for(i in index){
    #first machine at beta_{i}
    i1 <- beta_index[i]
    #second machine at beta_{i+1}
    i2 <- beta_index[i+1]
    #acceptance probability for the exhange
    #m$density is used - the log density of the "likelihood" or target function over prior
    alpha <- min(0,(beta[i+1]-beta_index[i])*(m$density(PT_machine[[i2]]$state$pars)-m$density(PT_machine[[i1]]$state$pars)))
    
    if(log(runif(1))<alpha) { #swap
      beta_index[i] <- i2
      beta_index[i+1] <- i1
      #this bit is a bit of a hack, but avoid exchanging the states accross "machines"
      #note that we still need to recalculate/update the density value
      #corresponding with the new "temperature"/beta
      PT_machine[[i2]]$beta <- beta[i]
      environment(PT_machine[[i2]]$model$density)$beta <- beta[i]
      PT_machine[[i2]]$state$density <- PT_machine[[i2]]$model$density(PT_machine[[i2]]$state$pars)
      PT_machine[[i1]]$beta <- beta[i+1]
      environment(PT_machine[[i1]]$model$density)$beta <- beta[i+1]
      PT_machine[[i1]]$state$density <- PT_machine[[i1]]$model$density(PT_machine[[i1]]$state$pars)
    }
  }
  even_step <- !even_step
  i_target <- beta_index[N_temp+1]
  all_state <- rep(0,N_temp+1)
  for(i in seq(N_temp+1)){
    all_state[i] <- PT_machine[[beta_index[i]]]$state$pars
  }
  x_res <- rbind(x_res, all_state)
  #x_res <- rbind(x_res, unlist(PT_machine[[i_target]]$state))
  index_res <- rbind(index_res, beta_index)
  beta_res <- c(beta_res, environment(PT_machine[[i_target]]$model$density)$beta)
}

par(mfrow=c(1,2))
target_d <- 16
plot(x_res[,target_d], type="l", col=grey(.8))
points(x_res[,target_d])
hist(x_res[,target_d], breaks = 150)


