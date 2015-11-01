 model{

	# Priors
	mu1 ~ dnorm(0,0.001)
	delta ~ dnorm(0,0.001)
	sigma ~ dunif(0,10)
	tau <- 1/(sigma*sigma)	
	
	# Likelihood
	for(i in 1:n){
		mu[i] <- mu1 + delta*x[i]
		y[i] ~ dnorm(mu[i], tau)
		residual[i] <- y[i] - mu[i]	# define residuals
		}

	# Derived Vars
	mu2 <- mu1 + delta
}

