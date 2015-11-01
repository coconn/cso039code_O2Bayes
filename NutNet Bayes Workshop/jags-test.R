# jags test 

## to use OpenBUGS instead, install "R2OpenBUGS" package
## replace "jags" names with "bugs"
## model should work unaltered

# Kery Ch 7: goal is Bayesian t-test, do group means differ significantly?
# generating data 
n1 <- 60
n2 <- 40
mu1 <- 105
mu2 <- 77.5
sigma <- 2.75


# adding error and combining into one dataset
n <- n1+n2
# indicator for pop
x <- rep(c(0,1), c(n1,n2))
# intercept (alpha) is grand mean
alpha <- mu1
# slope (beta) is difference in group means
beta <- mu2-mu1
# expected value is linear equation
E.y <- alpha + beta*x
# observed data is expected value plus error
y.obs <- rnorm(n=n, mean=E.y, sd=sigma)

boxplot(y.obs~x, col='grey')

# frequentist (lm) test
fit <- lm(y.obs~x)
summary(fit)
anova(fit)

# JAGS 
#install.packages('R2jags')
library(R2jags)
#setwd('~/Dropbox/Bayes')
###SET YOUR WORKING DIRECTORY HERE
setwd('~/Desktop/RESEARCH PROJECTS/cso040_O2Bayes/Pre-Workshop Mtg')

# Model update
sink('kery-JAGS.R')
cat(" model{

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
", fill=TRUE)
sink()

# data to pass to jags
jags.data <- list('x'=x,	# population indicator
'y'=y.obs,	# measurements (random normal)
'n'=n)	# number of observations

# function to create random inital starts
inits <- function(){list(mu1=rnorm(1), delta=rnorm(1), sigma=rlnorm(1))}
# e.g.
inits()

# parameters to estimate & track
params <- c("mu1","delta","mu2","residual")

# MCMC settings
# MCMC settings
nc <- 3	# number of chains
ni <- 3000	# number of draws from posterior for each chain
nb <- 1000 	# number of draws to discard as burn-in
nt <- 5	# thinning rate

# start the markov chains
out <- jags(data=jags.data, inits=inits, parameters=params, model="kery-JAGS.R", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

print(out, dig=3)

# plot residuals
plot(out$BUGSoutput$mean$residual,las=1)
abline(h=0)

# extract estimates of interest (ignoring residuals)
out.table <- data.frame(out$BUGSoutput$summary)[1:4,]
out.table$param <- rownames(out.table)

# main result: does delta differ from zero?
# "traditional" way: plot mu1 and mu2 with 95% credible interval
require(ggplot2)
p <- ggplot(out.table[3:4,],aes(y=mean,x=param))
p + geom_pointrange(aes(ymax=X97.5.,ymin=X2.5.))

# Bayesian: look at derived delta directly, does 95% overlap zero?
# can use a histogram of *all* the deltas from MCMC
delta.dat <- out$BUGSoutput$sims.list$delta
h <- qplot(delta.dat,geom='density')
h + xlim(-30,0)
