library(MASS)
library(ggplot2)
?geyser
ggplot(geyser) + geom_histogram(aes(x=waiting,stat(density)),binwidth=4)