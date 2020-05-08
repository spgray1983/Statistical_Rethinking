#Chapter 2 - Small Worlds and Large Worlds

#Quick compute of plausibilities:
ways = c(0, 3, 8, 9, 0)
ways/sum(ways)

#Grid Approximation Examples
#Define Grid:
p_grid = seq ( from = 0, to=1, length.out = 20)

#Define Prior
prior = rep(1,20)

#Compute likelihood
likelihood = dbinom(6, size = 9, prob = p_grid)

#Likelihood X Prior
unstd.posterior = likelihood*prior

#standardize poterior (i.e. sum to 1)
posterior = unstd.posterior / sum(unstd.posterior)

#Plot it
plot (p_grid, posterior, type = "b", xlab = "probability of water", 
      ylab = "posterior probability") 
mtext("20 points")

#Replicate with different priors
prior = ifelse(p_grid < .5, 0, 1)
prior = exp (-5*abs(p_grid-.5))

#Quadratic Approximation
#Intro to the quap fucntion in the rethinking package
#quap allows for a variety of different "regression models to be input

library (rethinking)
globe.qa = quap(alist(
  W ~ dbinom(W+L,p), #binomial likelihood (can vary)
  p ~ dunif(0,1)), #uniform prior (can vary)
  data = list (W=6, L=3))
#Display summary stats with precis command
precis(globe.qa)

#Analytical Calculation (using beta distribution)
W = 6
L = 3
curve(dbeta(x, W+1, L+1), from=0, to=1)
#Quadratic Approx
curve(dnorm(x, .67, .16), lty=2, add=TRUE)

#Markov Chain Monte Carlo
#simple Globe example
n_samples = 1000
p = rep(NA, n_samples)
p[1] = .5
W = 6
L = 3
for (i in 2:n_samples){
  p_new = rnorm(1, p[i-1],.1)
  if (p_new<0) p_new = abs(p_new)
  if (p_new>1) p_new = 2-p_new
  q0 = dbinom(W, W+L, p[i-1])
  q1 = dbinom(W, W+L, p_new)
  p[i] = ifelse(runif(1) < q1/q0, p_new, p[i-1])
}

#Plot it
dens(p, xlim=c(0,1))
curve(dbeta(x, W+1,L+1),lty=2, add=TRUE)
#######################################
#HOMEWORK
##################################

#2M1 & 2M2 HW
data1 = c("W","W","W")
data2 = c("W","W","W","L")
data3 = c("L","W","W","L","W","W","W")

n1 = length(data1)
n2 = length(data2)
n3 = length(data3)

W1 = sum(data1=="W")
W2 = sum(data2 =="W")
W3 = sum(data3 == "W")


#Create Grid
p_grid = seq ( from = 0, to=1, length.out = 20)

#Create Prior
#Prior1
#prior = rep (1,length(p_grid))
#Prior2
prior = ifelse(p_grid<.5,0,1)

#Compute likelihoods
likelihood1 = dbinom(W1, size = n1, prob = p_grid)
likelihood2 = dbinom(W2, size = n2, prob = p_grid)
likelihood3 = dbinom(W3, size = n3, prob = p_grid)

#Likelihood X Prior
unstd.posterior1 = likelihood1*prior
unstd.posterior2 = likelihood2*prior
unstd.posterior3 = likelihood3*prior

#standardize poterior (i.e. sum to 1)
posterior1 = unstd.posterior1 / sum(unstd.posterior1)
posterior2 = unstd.posterior2 / sum(unstd.posterior2)
posterior3 = unstd.posterior3 / sum(unstd.posterior3)

#Plot them
plot (p_grid, posterior1, type = "b", xlab = "probability of water", 
      ylab = "posterior probability") 
mtext("20 points")

plot (p_grid, posterior2, type = "b", xlab = "probability of water", 
      ylab = "posterior probability") 
mtext("20 points")

plot (p_grid, posterior3, type = "b", xlab = "probability of water", 
      ylab = "posterior probability") 
mtext("20 points")

#2M3
#P(Earth|land)
pg_earth = .5
pl_earth = .3
#prob globe is earth * prob l for earth globe divided by sum of prob land (both globes)

p = (pg_earth*pl_earth)/(pg_earth*pl_earth+(1*.5))

#2M4
