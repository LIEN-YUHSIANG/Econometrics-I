library(tidyverse)

data <- read.csv("game.csv")
head(data, 3)

# First model 
summary(lm(GPA ~ hours, data))$coefficient

ggplot(data, aes(hours, GPA)) + 
  geom_point() + 
  geom_smooth(method=lm)

# Second model with more variable
summary(lm(GPA ~ hours + alone, data))$coefficient

