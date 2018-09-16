#4e1
#The first line is the likelihood.
#4e2
#Their are two parameters in the posterior distribution
#4e4
#The second line is the linear model
#4e5
#There are 3 parameters in the posterior distribution

#4m1
#sample_mu <- rnorm(1e4, 0, 10)
#sample_sigma <- runif(1e4, 0, 10)
#prior_h <- rnorm(1e4, sample_mu, sample_sigma)
#dens(prior_h)

#4m2
#m4.2 <- map(
#  alist(
#    y ~ dnorm(mu, sigma),
#    mu ~ dnorm(0, 10)
#  )
#)

#4h1
library(rethinking)
data("Howell1")
d <- Howell1
#d2 <- d[d$age >=18,]
d2 <- d
d2$weight.s <- (d2$weight - mean(d2$weight))/sd(d2$weight)
d2$weight.s2 <- d2$weight.s^2
m4.1 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s + b2*weight.s2,
    a ~ dnorm(154, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight.s=weight.seq , weight.s2=weight.seq^2 )
mu <- link( m4.1 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.1 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )
#print(precis(m4.1))

#plot( height ~ weight.s , d2 , col=col.alpha(rangi2,0.5) )
#lines( weight.seq , mu.mean )
#shade( mu.PI , weight.seq )
#shade( height.PI , weight.seq )

preds.seq <- c(46.95, 43.72, 64.78, 32.59, 54.63)
preds.std <- (preds.seq-mean(d2$weight))/sd(d2$weight)
preds.use <- list(weight.s = preds.std, weight.s2 = preds.std^2)
new_mu <- link(m4.1, data = preds.use)
new_mu.mean <- apply(new_mu, 2, mean)
new_mu.HPDI <- apply(new_mu, 2, HPDI, prob =.89)
new_mu.PI <- apply(new_mu, 2, PI, prob =.89)