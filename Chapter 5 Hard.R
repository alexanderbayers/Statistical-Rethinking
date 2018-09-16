library(rethinking)
data(foxes)
d <- foxes

#Transform the data
d$avgfood.c <- (d$avgfood - mean(d$avgfood))/sd(d$avgfood)
d$groupsize.c <- (d$groupsize - mean(d$groupsize))/sd(d$groupsize)
d$area.c <- (d$area - mean(d$area))/sd(d$area)

problem5.1.bool = FALSE
if (problem5.1.bool){
  m5.1 <- map(
    alist(
      weight ~ dnorm(mu, sigma),
      mu <- a + b1*groupsize.c,
      a ~ dnorm(4.5, 5),
      b1 ~ dnorm(0, 100),
      sigma ~ dunif(0, 10)
    ),
    data = d
  )

  np.seq <- seq(from = -3, to = 3, length.out = 31)
  pred.data <- data.frame(groupsize.c = np.seq)
  mu <- link(m5.1, data = pred.data, n =1e4)
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI, prob = .95)
  
  
  plot(weight ~ groupsize.c, data = d, col=rangi2)
  lines(np.seq, mu.mean)
  lines(np.seq, mu.PI[1,], lty = 2)
  lines(np.seq, mu.PI[2,], lty = 2)
}



#Fit the model using using area
problem5.2.bool = FALSE
if (problem5.2.bool){
  m5.2 <- map(
    alist(
      weight ~ dnorm(mu, sigma),
      mu <- a + b1*area.c,
      a ~ dnorm(4.5, 5),
      b1 ~ dnorm(0, 100),
      sigma ~ dunif(0, 10)
    ),
    data = d
  )
  
  
  np.seq2 <- np.seq
  pred.data <- data.frame(area.c = np.seq2)
  mu2 <- link(m5.2, data = pred.data, n =1e4)
  mu.mean2 <- apply(mu2, 2, mean)
  mu.PI2 <- apply(mu2, 2, PI, prob = .95)
  
  
  plot(weight ~ area.c, data = d, col=rangi2)
  lines(np.seq2, mu.mean2)
  lines(np.seq2, mu.PI2[1,], lty = 2)
  lines(np.seq2, mu.PI2[2,], lty = 2)
}

#Looking at the data, groupize seems to correlate negatively with weight, being almost two
#deviations away from 0.  In contrast, area seems to have almost no effect on the groupsize

problem5.3.bool = FALSE
if (problem5.3.bool){
  #Now we build a model that will jointly fit area and groupsize
  m5.3 <- map(
    alist(
      weight ~ dnorm(mu, sigma),
      mu <- a + b1*area.c + b2*groupsize.c,
      a ~ dnorm(4.5, 100),
      b1 ~ dnorm(0, 10),
      b2 ~ dnorm(0,10),
      sigma ~ dunif(0, 10)
    ),
    data = d
  )
  
  #Graph by groupsize holding area constant at the mean
  area.avg <- mean(d$area.c)
  groupsize.avg <- mean(d$groupsize.c)
  area.seq <- np.seq
  groupsize.seq <- np.seq
  pred.data <- data.frame(groupsize.c = groupsize.seq, area.c = area.avg)
  pred.data.2 <- data.frame(groupsize.c = groupsize.avg, area.c = area.seq)
  
  #apply the model using the data for groupsize and average area
  mu5.3 <- link(m5.3, data = pred.data)
  mu5.3.mean <- apply(mu5.3, 2, mean)
  mu5.3.PI <- apply(mu5.3, 2, PI)
  
  #Generte the lines assuming average area
  R.sim <- sim(m5.3, data = pred.data, n =1e4)
  r.PI <- apply(R.sim, 2, PI)
  plot(weight ~ groupsize.c, data = d, col=rangi2)
  lines(groupsize.seq, mu5.3.mean)
  shade(mu5.3.PI, groupsize.seq)
  shade(r.PI, groupsize.seq)
  
  #apply the model using the data for the area and average groupsize
  mu5.3.2 <- link(m5.3, data = pred.data.2)
  mu5.3.mean.2 <- apply(mu5.3.2, 2, mean)
  mu5.3.PI.2 <- apply(mu5.3.2, 2, PI)
  
  R.sim.2 <- sim(m5.3, data = pred.data.2, n =1e4)
  r.PI.2 <- apply(R.sim.2, 2, PI)
  plot(weight ~ area.c, data = d, col=rangi2)
  lines(area.seq, mu5.3.mean.2)
  shade(mu5.3.PI.2, area.seq)
  shade(r.PI.2, area.seq)
  
  #Groupize and area are positiviely correlted, which results in masking.  Once we've removed the
  #the masking relationship both variables are equally important.
}

problem5.4.bool = FALSE
if (problem5.4.bool) {
  #Model 5.4 - here we're going to fit average weight as a function of averagefood and groupsize
  m5.4 <- map(
    alist(
      weight ~ dnorm(mu, sigma),
      mu <- a + b1*avgfood.c + b2*groupsize.c,
      a ~ dnorm(4.5, 100),
      b1 ~ dnorm(0, 10),
      b2 ~ dnorm(0,10),
      sigma ~ dunif(0, 10)
    ),
    data = d
  )
  
  #Graph by groupsize holding area constant at the mean
  avgfood.avg <- mean(d$avgfood.c)
  avgfood.seq <- np.seq
  pred.data <- data.frame(groupsize.c = groupsize.seq, avgfood.c = avgfood.seq)
  pred.data.2 <- data.frame(groupsize.c = groupsize.avg, avgfood.c = avgfood.seq)
  
  #apply the model using the data for groupsize and average area
  mu5.4 <- link(m5.4, data = pred.data)
  mu5.4.mean <- apply(mu5.4, 2, mean)
  mu5.4.PI <- apply(mu5.4, 2, PI)
  
  #Generte the lines assuming average area
  R.sim.5.4 <- sim(m5.4, data = pred.data, n =1e4)
  r.PI.5.4 <- apply(R.sim.5.4, 2, PI)
  plot(weight ~ groupsize.c, data = d, col=rangi2)
  lines(groupsize.seq, mu5.4.mean)
  shade(mu5.4.PI, groupsize.seq)
  shade(r.PI.5.4, groupsize.seq)
  
  #apply the model using the data for the area and average groupsize
  mu5.4.2 <- link(m5.4, data = pred.data.2)
  mu5.4.mean.2 <- apply(mu5.4.2, 2, mean)
  mu5.4.PI.2 <- apply(mu5.4.2, 2, PI)
  
  R.sim.5.4.2 <- sim(m5.4, data = pred.data.2, n =1e4)
  r.PI.5.4.2 <- apply(R.sim.5.4.2, 2, PI)
  plot(weight ~ avgfood.c, data = d, col=rangi2)
  lines(avgfood.seq, mu5.4.mean.2)
  shade(mu5.4.PI.2, avgfood.seq)
  shade(r.PI.5.4.2, avgfood.seq)
}

m5.5 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b1*avgfood.c + b2*groupsize.c + b3*area.c,
    a ~ dnorm(4.5, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0,10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)

#The correlation between avgfood and area is quite high - this means the model has trouble
#figuring out which one to assign the value to and hence we have relatively higher variance
#On the basis of MSFE its's quite hard to determine between them, but I think average food
#is a better predictor (but it's relatively marginal)
