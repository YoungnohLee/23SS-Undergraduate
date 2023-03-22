install.packages("R2OpenBUGS")
library(R2OpenBUGS)

data = list(n=100, m=100, x=10, a=1, b=1)
inits = function(){list(theta=0.5, xtilde=10)}

##----
# betabinom model compiled by WinBUGS
model{ #betabinom.txt
  x ~ dbin(theta, n) # Model the data
  xtilde ~ dbin(theta, m) # Prediction of future binomial
  theta ~ dbeta(a, b) # The prior
  prob <- step(xtilde - 20) # Pred prob that xtilde >= 20
}
list(n = 100, m = 100, x = 10, a = 1, b = 1) #data
list(theta = 0.5, xtilde =10) #starting/initial values
list(theta = 0.1, xtilde =9) #starting/initial values for chain 2
list(theta = 0.9, xtilde =11) #starting/initial values for chain 3
##----

getwd()
betabinom.sim <- bugs(data, inits, model.file = "betabinom.txt",
parameters = c("theta","xtilde"), n.chains=1, n.iter=20000, debug=T)
# since betabinom.txt is located at current working directory, saved by WinBUGS,
# we can use `bugs` module to simulate MCMC
print(betabinom.sim)
plot(betabinom.sim)

##----Result from above MCMC----

Current: 1 chains, each with 20000 iterations (first 10000 discarded)
Cumulative: n.sims = 10000 iterations saved
mean  sd 2.5% 25%  50%  75% 97.5%
theta     0.1 0.0  0.1 0.1  0.1  0.1   0.2
xtilde   10.8 4.3  4.0 8.0 10.0 14.0  20.0
deviance  5.1 1.4  4.1 4.2  4.5  5.4   9.2

DIC info (using the rule, pD = Dbar-Dhat)
pD = 0.9 and DIC = 6.0
DIC is an estimate of expected predictive error (lower deviance is better).

##----

betabinom.sim <- bugs(data, inits, model.file="betabinom.txt",
parameters=c("theta","xtilde"), n.chains=1, n.iter=20000,codaPkg=T)

codaobject <- read.bugs(betabinom.sim)
plot(codaobject)
