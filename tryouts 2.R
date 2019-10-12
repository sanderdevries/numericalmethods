library(tidyverse)
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
ggplot(data = annoying) + 
  geom_point(mapping = aes(x= '1', y='2'))
annoying$'3' <- annoying$'2' / annoying$'1'
names(annoying)[1:3] <- c("one", "two", "three")
