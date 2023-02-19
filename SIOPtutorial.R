## GMB tutorial SIOP - 4/29/22

data <- read.csv("train.csv")

summary(lm(Overall_Rating~Technical_Skills,data))
