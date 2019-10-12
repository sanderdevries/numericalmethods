library(nycflights13)
library(tidyverse)
library(readxl)
library(lubridate) #contains functions that are easy to work with if your data contains date and time
m.files <- list.files("male data", full.names = TRUE) #list.files creates a list that contains all data in the folder 'male data', by setting full names true it is categorized in alphabetical order
f1 <- read_excel(m.files[1]) #reads the first term of m.files

force.type <- function(d){ #creates a function that changes all characters in numerical 
  nm <- names(d) #nm contains al variables in dataset d
  if (is.element("WRank",nm)) d$WRank <- as.numeric(d$WRank) #if WRank is one of the observations in d, then it changes to numerical
  if (is.element("LRank",nm)) d$LRank <- as.numeric(d$LRank)
  if (is.element("B365W",nm)) d$B365W <- as.numeric(d$B365W)
  if (is.element("B365L",nm)) d$B365L <- as.numeric(d$B365L)
  if (is.element("WPts",nm)) d$WPts <- as.numeric(d$WPts)
  if (is.element("LPts",nm)) d$LPts <- as.numeric(d$LPts)
  if (is.element("LBW",nm)) d$LBW <- as.numeric(d$LBW)
  if (is.element("LBL",nm)) d$LBL <- as.numeric(d$LBL)
  d
}
m.tennis <- force.type(f1) #transforms all necessary characters in doubles
for(i in 2:length(m.files)){ #for all remaining files
  f <- read_excel(m.files[i]) #file 2, ...., 2017
  f <- force.type(f) #again transform all variables
  m.tennis <- bind_rows(m.tennis, f) #bind them all together
}
m.tennis$Sex <- "male" #create the variable 'male' and add it to the dataset

f.files <- list.files("female data", full.names = TRUE) #now do the same for women's data
m1 <- read_excel(f.files[1])
f.tennis <- force.type(m1)
for (i in 2: length(f.files)) {
  f <- read_excel(f.files[i])
  f <- force.type(f)
  f.tennis <- bind_rows(f.tennis, f)
}
f.tennis$sex <- "female"

tennis.data <- bind_rows(m.tennis, f.tennis) #bind all data together
tennis.data <- tennis.data[,c(55,1:54,56,57)] #change order s.t. gender is first column
save(tennis.data, file="tennis.Rda") #save data in the working directory (wd)

tennis.data <- select(tennis.data, Sex:Comment, B365W, WTA, Tier) 
set.seed(662288)
e <- sample(1: nrow(tennis.data), floor(nrow(tennis.data)/2)) #sample from 'n' observations n/2 rows
part1 <- tennis.data[e,] #first half which contains all rows corresponding to sample e
part2 <- tennis.data[-e,]#second half
names(part1)[10:29] <- c("Best.of","PlayerA","PlayerB",
                         "RankA","RankB","A1","B1","A2","B2","A3","B3","A4","B4","A5","B5",
                         "SetsA","SetsB","Comment","B365A","B365B")
names(part2)[10:20] <- c("Best.of","PlayerB","PlayerA",
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
lapply(tennis.data, summary) #apply summary over the list of variables of the data
tennis.data %>% filter(Series == "Grand Slam") %>% #exclude all other series
  group_by(Tournament) %>% #group all tournaments (does not change the order of the data)
  summarise('percentage of 5 sets' = sum(!is.na(A5))/n()) #exclude all matches in which no result is available of the 5th set
