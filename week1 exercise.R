library(readxl)
library(tidyverse)

m.data <- list.files("male data", full.names = TRUE)
f.data <- list.files("female data", full.names = TRUE)

m1 <- read_excel(m.data[1])
forceTypes <- function(x){
  names <- names(x)
  if (is.element("Wrank", names)) x$Wrank <- as.numeric(x$Wrank)
  if (is.element("Lrank", names)) x$LRank <- as.numeric(x$Lrank)
  if (is.element("WRank",names)) x$WRank <- as.numeric(x$WRank) #if WRank is one of the observations in x, then it changes to numerical
  if (is.element("LRank",names)) x$LRank <- as.numeric(x$LRank)
  if (is.element("B365W",names)) x$B365W <- as.numeric(x$B365W)
  if (is.element("B365L",names)) x$B365L <- as.numeric(x$B365L)
  if (is.element("WPts",names)) x$WPts <- as.numeric(x$WPts)
  if (is.element("LPts",names)) x$LPts <- as.numeric(x$LPts)
  if (is.element("LBW",names)) x$LBW <- as.numeric(x$LBW)
  if (is.element("LBL",names)) x$LBL <- as.numeric(x$LBL)
  x
}
m.tennis <- forceTypes(m1)

for(i in 2:length(m.data)){
  m <- read_excel(m.data[i])
  m <- forceTypes(m)
  m.tennis <- bind_rows(m.tennis,m)
}

f1 <- read_excel(f.data[2])
f.tennis <- forceTypes(f1)
for(i in 2:length(f.data)){
  f <- read_excel(f.data[i])
  f <- forceTypes(f)
  f.tennis <- bind_rows(f.tennis, f)
}

m.tennis$sex <- 1
f.tennis$sex <- 0
tennis.data <- bind_rows(m.tennis, f.tennis)
tennis.data <- tennis.data[, c(55, 1:54, 56, 57)]
save(tennis.data, file = "tennis.Rda")

set.seed(662288)
e1 <- sample(1: nrow(tennis.data), floor(nrow(tennis.data)/2))
firstHalf <- tennis.data[e1,]
secondHalf <- tennis.data[-e1,]
names(firstHalf)[10:29] <- c("Best.of","PlayerA","PlayerB",
                         "RankA","RankB","A1","B1","A2","B2","A3","B3","A4","B4","A5","B5",
                         "SetsA","SetsB","Comment","B365A","B365B")
names(secondHalf)[10:29] <- c("Best.of","PlayerB","PlayerA",
                         "RankB","RankA","B1","A1","B2","A2","B3","A3","B4","A4","B5","A5",
                         "SetsB","SetsA","Comment","B365B","B365A")
firstHalf$winner <- "A"
secondHalf$winner <- "B"
tennis.data.relabeled <- bind_rows(firstHalf, secondHalf)
table(tennis.data.relabeled$Comment)
tennis.data <- filter(tennis.data.relabeled, Comment == "Completed")
lapply(tennis.data, summary)
tennis.data %>% filter(Series == "Grand Slam")   %>% 
  group_by(Tournament)   %>% 
  summarise('A' = sum(!is.na(A5))/n()) #if A5 is not available, !is.na(A5) == FALSE, i.e. we have the number of NA's
