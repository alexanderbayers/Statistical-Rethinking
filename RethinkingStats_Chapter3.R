## R code 3.27
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

#My Problem
#plot(samples)

# add up posterior probability where p < 0.5
print(sum(samples < .2)/1e4)
print(sum(samples > .8)/1e4)
print(sum(samples < .8 & samples > .2)/1e4)
print(quantile(samples, .2))
print(quantile(samples, .8))
print(PI(samples, .66))
print(HPDI(samples, .66))