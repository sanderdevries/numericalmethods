carsData <- read.csv("Cars_CBC_all.csv")
ggplot(data = carsData) + 
  geompoint(mapping = aes(x = id, y =age))