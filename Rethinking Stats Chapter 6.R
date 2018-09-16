#6.1E
#Uncertainty should be continuous, increasing in the numberr of variables, and additive

#6.2
#p <- c(.3, .7)
#print(-sum(p * log(p)))

#6.3
#p <- c(.2, .25, .25, .3)
#print(-sum(p * log(p)))

#6.4
#p <- c(1/3, 1/3, 1/3)
#print(-sum(p * log(p)))

#6M1
#AIC-Training deviance + 2x the parameters used in fitting.  It is assumes priors are flat, posterior distribution is
#multivariate normal, and the sample size is much greater than the number of parameters.
#DIC- DIC allows for an informative prior
#WAIC-WAIC most general, allows for nonnormality

#6M2
#Model averaging is averaging the forecasts of the separate parameter estimates.  Model selection is choosing the one 
#with the best forecast.  Model averaing guards against overconfidence, but loses the some interpretability.

#6M3
#First, information criteria don't average based on the number of observations so it's nonsensical.  Second, there
#will be different degrees of uner/overfitting based on the amount of variables that are used.

#6M4
#As a prior becomes more and more concentrated the effective number of parameters goes down

#6M5
#Informative priors reduce the flexibility because they push the fit cloesr and closer to the prior.

#6M6
#Overly informative priors push the result to close to the prior, which results in underfitting.

#6H1
library(rethinking)
data("Howell1")
d <- Howell1
d$age.c <- ((d$age - mean(d$age))/sd(d$age))
set.seed(1000)
i <- sample(1:nrow(d), size = nrow(d)/2)
d1 <- d[i,]
d2 <- d[-i,]

a1_start <- mean(d1$height)

m6.1 <- map(
  alist(
      height ~ dnorm(mu, sigma),
      mu <- a1 + b1*age.c,
      a1 <- dnorm(a1_start, 50),
      b1 <- dnorm(20, 20),
      sigma  <- dunif(0, 30)
  ),
  data = d1
)
summary(m6.1)

m6.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a1 + b1 * age.c + b2 * age.c^2,
    a1 <- dnorm(a1_start, 50),
    b1 <- dnorm(20, 20),
    b2 <- dnorm(0, 100),
    sigma  <- dunif(0, 30)
  ),
  data = d1
)
summary(m6.2)

m6.3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a1 + b1 * age.c + b2 * age.c^2 + b3 * age.c^3,
    a1 <- dnorm(a1_start, 50),
    b1 <- dnorm(20, 20),
    b2 <- dnorm(0, 100),
    b3 <- dnorm(0, 100),
    sigma  <- dunif(0, 30)
  ),
  data = d1
)
summary(m6.3)

m6.4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a1 + b1 * age.c + b2 * age.c^2 + b3 * age.c^3 + b4 * age.c^4,
    a1 <- dnorm(a1_start, 50),
    b1 <- dnorm(20, 20),
    b2 <- dnorm(0, 100),
    b3 <- dnorm(0, 100),
    b4 <- dnorm(0, 100),
    sigma  <- dunif(0, 30)
  ),
  data = d1
)
summary(m6.4)

m6.5 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a1 + b1 * age.c + b2 * age.c^2 + b3 * age.c^3 + b4 * age.c^4 + b5 * age.c^5,
    a1 <- dnorm(a1_start, 50),
    b1 <- dnorm(20, 20),
    b2 <- dnorm(0, 100),
    b3 <- dnorm(0, 100),
    b4 <- dnorm(0, 100),
    b5 <- dnorm(0, 100),
    sigma  <- dunif(0, 30)
  ),
  data = d1
)
summary(m6.5)

m6.6 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a1 + b1 * age.c + b2 * age.c^2 + b3 * age.c^3 + b4 * age.c^4 + b5 * age.c^5 + b6 * age.c^6,
    a1 <- dnorm(a1_start, 50),
    b1 <- dnorm(20, 20),
    b2 <- dnorm(0, 100),
    b3 <- dnorm(0, 100),
    b4 <- dnorm(0, 100),
    b5 <- dnorm(0, 100),
    b6 <- dnorm(0, 100),
    sigma  <- dunif(0, 30)
  ),
  data = d1
)
summary(m6.6)

m6_ensemble <- ensemble(m6.1, m6.2, m6.3, m6.4, m6.5, m6.6, data = kung.predict)
mu_ensemble <- apply(m6_ensemble$link, 2, mean)
mu_PI_ensemble <- apply(m6_ensemble$link, 2, PI, .97)
#lines(kung.seq, mu_ensemble, lty = 2)
#shade(mu_PI_ensemble, kung.seq)

#assumptions for model 6.1
pred.m6.1 <- link(m6.1, data = kung.predict)
mu <- apply(pred.m6.1, 2, mean)
mu.PI <- apply(pred.m6.1, 2, PI, .97)
plot(height ~ age.c, data = d1, col = rangi2)
lines(kung.seq, mu, lty =2)
shade(mu.PI, kung.seq)
lines(kung.seq, mu_ensemble, lty = 2)
shade(mu_PI_ensemble, kung.seq)

#assumptions for model 6.2
pred.m6.2 <- link(m6.2, data = kung.predict)
mu <- apply(pred.m6.2, 2, mean)
mu.PI <- apply(pred.m6.2, 2, PI, .97)
plot(height ~ age.c, data = d1, col = rangi2)
lines(kung.seq, mu, lty =2)
shade(mu.PI, kung.seq)
lines(kung.seq, mu_ensemble, lty = 2)
shade(mu_PI_ensemble, kung.seq)

#assumptions for model 6.3
pred.m6.3 <- link(m6.3, data = kung.predict)
mu <- apply(pred.m6.3, 2, mean)
mu.PI <- apply(pred.m6.3, 2, PI, .97)
plot(height ~ age.c, data = d1, col = rangi2)
lines(kung.seq, mu, lty =2)
shade(mu.PI, kung.seq)
lines(kung.seq, mu_ensemble, lty = 2)
shade(mu_PI_ensemble, kung.seq)

#assumptions for model 6.4
pred.m6.4 <- link(m6.4, data = kung.predict)
mu <- apply(pred.m6.4, 2, mean)
mu.PI <- apply(pred.m6.4, 2, PI, .97)
plot(height ~ age.c, data = d1, col = rangi2)
lines(kung.seq, mu, lty =2)
shade(mu.PI, kung.seq)
lines(kung.seq, mu_ensemble, lty = 2)
shade(mu_PI_ensemble, kung.seq)

#assumptions for model 6.5
pred.m6.5 <- link(m6.5, data = kung.predict)
mu <- apply(pred.m6.5, 2, mean)
mu.PI <- apply(pred.m6.5, 2, PI, .97)
plot(height ~ age.c, data = d1, col = rangi2)
lines(kung.seq, mu, lty =2)
shade(mu.PI, kung.seq)
lines(kung.seq, mu_ensemble, lty = 2)
shade(mu_PI_ensemble, kung.seq)

#assumptions for model 6.6
pred.m6.6 <- link(m6.6, data = kung.predict)
mu <- apply(pred.m6.6, 2, mean)
mu.PI <- apply(pred.m6.6, 2, PI, .97)
plot(height ~ age.c, data = d1, col = rangi2)
lines(kung.seq, mu, lty =2)
shade(mu.PI, kung.seq)
lines(kung.seq, mu_ensemble, lty = 2)
shade(mu_PI_ensemble, kung.seq)
compare_table <- compare(m6.1,m6.2,m6.3,m6.4,m6.5, m6.6)

#Problem 6.4
pred.m6.1.oos <- link(m6.1, data = d2)
mu.6.1.oos <- apply(pred.m6.1.oos, 2, mean)
print(sum(dnorm(d2$height, mu.6.1.oos, m6.1@coef["sigma"], log = TRUE)))

pred.m6.2.oos <- link(m6.2, data = d2)
mu.6.2.oos <- apply(pred.m6.2.oos, 2, mean)
print(sum(dnorm(d2$height, mu.6.2.oos, m6.2@coef["sigma"], log = TRUE)))

pred.m6.3.oos <- link(m6.3, data = d2)
mu.6.3.oos <- apply(pred.m6.3.oos, 2, mean)
print(sum(dnorm(d2$height, mu.6.3.oos, m6.3@coef["sigma"], log = TRUE)))

pred.m6.4.oos <- link(m6.4, data = d2)
mu.6.4.oos <- apply(pred.m6.4.oos, 2, mean)
print(sum(dnorm(d2$height, mu.6.4.oos, m6.4@coef["sigma"], log = TRUE)))

pred.m6.5.oos <- link(m6.5, data = d2)
mu.6.5.oos <- apply(pred.m6.5.oos, 2, mean)
print(sum(dnorm(d2$height, mu.6.5.oos, m6.5@coef["sigma"], log = TRUE)))

pred.m6.6.oos <- link(m6.6, data = d2)
mu.6.6.oos <- apply(pred.m6.6.oos, 2, mean)
print(sum(dnorm(d2$height, mu.6.6.oos, m6.6@coef["sigma"], log = TRUE)))

#Model 6.4 (the quartic model) seems to fit the data best.  This accords with the WAIC,
#and the differences are pretty similar.  M6.4 predicts the best out of sample,
#exactly in line with the WAIC.

m6.7 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a1 + b1 * age.c + b2 * age.c^2 + b3 * age.c^3 + b4 * age.c^4 + b5 * age.c^5 + b6 * age.c^6,
    a1 <- dnorm(a1_start, 50),
    b1 <- dnorm(0, 5),
    b2 <- dnorm(0, 5),
    b3 <- dnorm(0, 5),
    b4 <- dnorm(0, 5),
    b5 <- dnorm(0, 5),
    b6 <- dnorm(0, 5),
    sigma  <- dunif(0, 30)
  ),
  data = d1
)
summary(m6.7)
