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
