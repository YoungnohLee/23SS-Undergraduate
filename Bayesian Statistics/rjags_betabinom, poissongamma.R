install.packages("rjags")
library(rjags)

# for JAGS syntax, refer to the following manual
https://www.stats.ox.ac.uk/~nicholls/MScMCMC15/jags_user_manual.pdf

##----

n=20; x=12; alpha=1; beta=1
model_string <- "model{
  x ~ dbinom(theta,n) # likelihood
  theta ~ dbeta(alpha, beta) # prior
  xtilde ~ dbern(theta) # predictive likelihood
  prob <- equals(xtilde,1) # predictive probability # equals: test for equality(logical)
}"
betabin_model <- jags.model(textConnection(model_string),
                            data=list(x=x,n=n,alpha=alpha,beta=beta))
update(betabin_model,10000); # burn-in for 10000 samples
postsamp <- coda.samples(betabin_model,
                         variable.names = c("theta","xtilde","prob"),
                         n.iter=10000)
summary(postsamp)
plot(postsamp)

##----
# Count Data : Poisson-Gamma Model
# 1) Grid approximation. exact built-in used for X(data)

a<-0.5; b<-0.5; n<-20; x<-sum(rpois(n,0.5)); grid<-seq(0.01,2,.01)
like<-dpois(x,n*grid); like<-like/sum(like)
prior<-dgamma(grid,a,b); prior<-prior/sum(prior)
post<-like*prior; post<-post/sum(post) #post<-dgamma(grid,x+a,n+b)
plot(grid,like,type="l",lty=2, col=1, xlab="theta",ylab="Density")
lines(grid,prior,col="blue"); lines(grid,post,lwd=2,col="red")
legend("topright",c("Likelihood","Prior","Posterior"),
       lwd=c(1,1,2),lty=c(2,1,1),col=c(1,"blue","red"))
(x+a)/(n+b); sqrt(x+a)/(n+b) #posterior mean & s.d.
qgamma(c(0.025,0.975), x+a, n+b) #posterior quantiles

##----
# 2) Monte Carlo Approximation using exact built-in fct
n.samples <- 10000; set.seed(10)
postsamp <- rgamma(n.samples,x+a,n+b)
hist(postsamp,breaks=25,xlim=0:1,main="Posterior density",freq=F)
lines(density(postsamp),lty=2,col="red")
lines(grid,dgamma(grid,x+a,n+b), lty=1, col="green")
legend("topright",c("Density estimate","Exact density"),
       lwd=c(1,1),lty=c(2,1),col=c("red","green"))
mean(postsamp); sd(postsamp)
quantile(postsamp,c(0.025,0.975))

##----
# 3) Rejection Sampling: simulating Gamma dist. 
gamma.sim <- function(alpha) {
  fx <- function(x) x^(alpha-1)*exp(-x)/gamma(alpha)
  g <- function(x) (1/alpha)*exp(-x/alpha)
  #M <- 3^(3/2)/sqrt(2*pi*exp(1))
  M <-optimize(f=function(x){fx(x)/g(x)},
               maximum=T,interval=c(0,100))$objective
  while (TRUE) {
    Y <- -log(runif(1))*alpha; V <- runif(1, 0, M*g(Y))
    if (V < fx(Y)) return(Y)} }
nsim<-10^4; alpha=3/2; gamma.rng<-replicate(nsim,gamma.sim(alpha))
hist(gamma.rng,nclass=20,freq=F,ylim=c(0,0.5),col="gray")
curve(dgamma(x,alpha,1),0,10,add=TRUE, col="red4",lwd=2)
#postsamp<-replicate(nsim,gamma.sim(x+a))/(n+b)

