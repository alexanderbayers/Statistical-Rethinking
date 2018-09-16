## R code 2.3
# define grid
p_grid <- seq( from=0 , to=1 , length.out=100 )

# define prior
#prior <- rep( 1 , 100 )
prior <- c(rep(0, 50), rep(2, 50))

# compute likelihood at each value in grid
likelihood <- dbinom( 8 , size=15 , prob=p_grid )

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

## R code 2.4
#plot( p_grid , posterior , type="b" ,
#      xlab="probability of water" , ylab="posterior probability" )
#mtext( "100 points" )

samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
HPDI(samples, 0.9)
w <- rbinom(1e4, size = 15, prob = samples)
z <- rbinom(1e4, size = 9, prob = samples)
sum(w == 8)/1e4
sum(z==6)/1e4