## SIOP 2023 Machine Learning Competition

## description is whack/poor: https://eval.ai/web/challenges/challenge-page/1937/overview

## Alicia recommended qualitative resource: https://www.thematicanalysis.net/

train <- read.csv("Boston2023/train_pub.csv")
library(descr)
library(tidyverse)
freq(train$rating_chooses_appropriate_action)
freq(train$rating_commits_to_action)
freq(train$rating_gathers_information)
freq(train$rating_identifies_issues_opportunities)
freq(train$rating_interprets_information)
freq(train$rating_involves_others)
freq(train$rating_decision_making_final_score)
cor(train[2:8], use="complete.obs")

train$NA_count<-rowSums(is.na(train[9:25]))
freq(train$NA_count)

train$predictor <- paste(train$text_exercise_4, 
                         train$text_exercise_5,
                         train$text_exercise_6, 
                         train$text_exercise_7,
                         train$text_exercise_8, 
                         train$text_exercise_9,
                         train$text_exercise_10, 
                         train$text_exercise_11,
                         train$text_exercise_12, 
                         train$text_exercise_13,
                         train$text_exercise_14, 
                         train$text_exercise_15,
                         train$text_exercise_16, 
                         train$text_exercise_17,
                         train$text_exercise_18, 
                         train$text_exercise_19,
                         train$text_exercise_final)

library(stringr)
train$predictor <- gsub('NA','',train$predictor)
train$criterion <- round(rowMeans(train[2:7], na.rm=TRUE), 2)
freq(train$criterion)
data<-train%>% select(predictor, rating_chooses_appropriate_action,
                      rating_commits_to_action, 
                      rating_gathers_information,
                      rating_identifies_issues_opportunities,
                      rating_interprets_information,
                      rating_involves_others,
                      rating_decision_making_final_score)
write_csv(data, "Boston2023/firstTry.csv")


