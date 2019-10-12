library(readxl)
library(tidyverse)
load("tennis.Rda")

tennis.data <- select(tennis.data, Sex:Comment, B365W, WTA, Tier) 
set.seed(662288)
e <- sample(1: nrow(tennis.data), floor(nrow(tennis.data)/2)) #sample from 'n' observations n/2 rows
part1 <- tennis.data[e,] #first half which contains all rows corresponding to sample e
part2 <- tennis.data[-e,]#second half
names(part1)[10:29] <- c("Best.of","PlayerA","PlayerB",
                         "RankA","RankB","A1","B1","A2","B2","A3","B3","A4","B4","A5","B5",
                         "SetsA","SetsB","Comment","B365A","B365B")
names(part2)[10:29] <- c("Best.of","PlayerB","PlayerA",
                         "RankB","RankA","B1","A1","B2","A2","B3","A3","B4","A4","B5","A5",
                         "SetsB","SetsA","Comment","B365B","B365A")
part1$winner <- "A"
part2$winner <- "B"
tennis.data.relabeled <- bind_rows(part1, part2)
mean(tennis.data.relabeled$winner=="A") #We started by choosing random matches and create two parts. In the first part we called the winner playerA and the loser player B
#and change all the other labels accordingly. In the second part we label all the winners playerB and losers player A.
#then by binding them together we obtain data in which the winner is 50% player A and 50% player B

table(tennis.data.relabeled$Comment) #inspect number of retired matches
tennis.data <- filter(tennis.data.relabeled, Comment == "Completed") #only use completed matches
tennis.data$Date <- as.Date(tennis.data$Date)
lapply(tennis.data, summary)


p <- c(0,0)
loglik <- function(p){
  b0 <- p[1]
  b1 <- p[2]
  i <- 1
  sum <- -1*(log(1+exp(-1*b0)(tennis.data$RankA[i]/tennis.data$RankB[i])^(-b1*b1)))
  for(i in 2: length(tennis.data$RankA)){
    sum <- sum - 1*(log(1+exp(-1*b0)(tennis.data$RankA[i]/tennis.data$RankB[i])^(-1*b1)))
    i <- i+1
  }
  return(sum)
}
loglik(p)

