data(Howell1)
d <- Howell1


m4.1 <- map(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b*log(weight),
        a ~ dnorm(178, 100),
        b ~ dnorm(0, 100),
        sigma ~ dunif(0, 50)
    ),
    data <- d
)

#print(precis(m4.1))



weight.seq <- seq(from =0, to = 70, length.out = 70)
pred_data = list(weight = weight.seq)
mu <- link(m4.1, data = pred_data)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = .97)
sim.height <- sim(m4.1, data = pred_data)
height.HPDI <- apply(sim.height, 2, HPDI, prob = .97)

plot(height ~ weight, data = Howell1, col = col.alpha(rangi2, 0.4))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(height.HPDI, weight.seq)