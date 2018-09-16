#Problem 4.4
library(rethinking)
data("Howell1")
d <- Howell1
d$log_weight <- log(d$weight)
#d$log_standardized <- (d$log_weight - mean(d$log_weight))/sd(log_weight)
#plot(d$log_weight)

m4.3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*log_weight,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 100),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

weight.seq <- seq( from=0 , to=70 , length.out=700)
mu <- link(m4.3, pred_data)

print(precis(m4.3))