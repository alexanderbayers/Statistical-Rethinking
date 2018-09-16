library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age <18,]
print(nrow(d2))

d2$weight.s <- (d2$weight - mean(d2$weight))/sd(d2$weight)
m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s + b2*weight.s2,
    a ~ dnorm(108, 100),
    b1 ~ dnorm(24, 10),
    sigma ~ dunif(8, 50)
  ),
  data = d2
)
#precis(m4.2)
#post <- extract.samples(m4.2)
#print(mean(post$b1)*10/sd(d2$weight))

weight.seq <- seq( from=-3 , to=3 , length.out=30 )
pred_dat <- list( weight.s=weight.seq)
mu <- link( m4.2 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.2 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

plot( height ~ weight.s , d2 , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )

#Their looks to  be a nonlinear relationship.  Definitely seems like some kind of quadratic fit.