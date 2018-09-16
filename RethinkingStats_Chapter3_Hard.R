library(rethinking)
data(homeworkch3)
print(sum(birth1)+sum(birth2))

p_grid <- seq(from=0, to=1, length = 1000)
likelihood <- dbinom(111, 200, prob = p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
print(p_grid[which.max(posterior)])
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
print(HPDI(samples, .5))
print(HPDI(samples, .89))
print(HPDI(samples, .97))
w <- rbinom(1e4, size = 49, prob = samples)
simplehist(w)
#print(sum(w>sum(birth1)))/1e4
