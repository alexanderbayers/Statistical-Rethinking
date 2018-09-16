library(rethinking)
data(foxes)
d <- foxes

m5.2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b1*groupsize + b2*area,
    a ~ dnorm(4.45, 100),
    b1 ~ dnorm(0.02, 100),
    b2 ~ dnorm(0, 100),
    sigma ~ dunif(0, 10)
  ),
  data = d
)

precis(m5.2)