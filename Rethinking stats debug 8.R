island.index <- seq(from =  1, to = 10)
island.pop <- sample(island.index, 10)
island.frame <- data.frame(island.name, island.index, island.pop)

num_weeks <- 1e2
positions <- rep(0, num_weeks)
current <- 10
for (i in 1:num_weeks){
  
  #record current position
  positions[i] <- current
  
  #flip coin to generate proposal
  proposal <- current + sample(c(-1, 1), size = 1)
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  
  prob_move <- (subset(island.frame, island.index == proposal, select = c("island.pop")) / subset(island.frame, island.index == current, select = c("island.pop")))$island.pop
  current <- ifelse(runif(1) < prob_move, proposal, current)
}
