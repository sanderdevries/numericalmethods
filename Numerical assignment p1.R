load("tennis (1).Rda")
library(tidyverse)

tennis.data <- tennis.data[complete.cases(tennis.data[,13:14]),] #drop NA's from data

tennis.data$dummy <- as.numeric(tennis.data$Winner=="A") #creates a dummy which equals 1 if A wins
data.ranks <- cbind(tennis.data$RankA, tennis.data$RankB, tennis.data$dummy) #a dataframe of just the ranks and the winner

loglik <- function(p,z){ #creates the loglikelihood function. Input are parameters p and data z
  b0 <- p[1]
  b1 <- p[2]
  rankA <- z[,1]
  rankB <- z[,2]
  winnerA <- z[,3]
  
  pdf <- 1/(1 + exp(-b0 - b1*log(rankA/rankB)))

  likelihood <- pdf^winnerA*(1-pdf)^(1-winnerA)
  sum(log(likelihood))
}
p <- c(1,1) #set the initial parameters
 

m <- optim(p, loglik, 
           control = list(fnscale = -1), hessian = TRUE, z= data.ranks)
m
rbind(m$par,sqrt(diag(solve(-m$hessian)))) #computes a table with the parameters and corresponding s.e.

