## SIOP 2023 Machine Learning Competition

## description is whack/poor: https://eval.ai/web/challenges/challenge-page/1937/overview
#install.packages("tidyverse")
## Alicia recommended qualitative resource: https://www.thematicanalysis.net/

train <- read.csv("D:\\ML SIOP\\2023 ML SIOP\\Data\\Data2\\test_pub.csv")
library(descr)
library(tidyverse)
# freq(train$rating_chooses_appropriate_action)
# freq(train$rating_commits_to_action)
# freq(train$rating_gathers_information)
# freq(train$rating_identifies_issues_opportunities)
# freq(train$rating_interprets_information)
# freq(train$rating_involves_others)
# freq(train$rating_decision_making_final_score)
# cor(train[2:8], use="complete.obs")

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
#
library(qdap)
library(tm)
#Final Text
text_exercise_final <- train$text_exercise_final
t_e_f_VectorSource <- VectorSource(text_exercise_final)
TEF_Volatile_Corpus <- VCorpus(t_e_f_VectorSource)
str(TEF_Volatile_Corpus)
df_tef_corpus <- data.frame(num = c(1,2,3), doc_id = row.names(train), text= c(train$text_exercise_final))
df_tef_corpus
df_tef_source <- DataframeSource(df_tef_corpus[,2:3])
df_tef_corpus_dfs <- VCorpus(df_tef_source)
df_tef_corpus_dfs
str(df_tef_corpus_dfs)
freq_terms(df_tef_corpus_dfs)
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)
}
clean_corp <- clean_corpus(df_tef_corpus_dfs)
B_dtm <- DocumentTermMatrix(clean_corp)
B_dtm
B_M <- as.matrix(B_dtm)
dim(B_M)
B_M_Sparse <- removeSparseTerms(B_dtm, 0.98)
B_M_Sparse
b_Matrix <- as.matrix(B_M_Sparse)
dim(b_Matrix)


train$criterion <- b_Matrix


# round(rowMeans(train[2:7], na.rm=TRUE), 2)
#/
freq(train$criterion)
data123<-train%>% select(predictor, rating_chooses_appropriate_action,
                      rating_commits_to_action,
                      rating_gathers_information,
                      rating_identifies_issues_opportunities,
                      rating_interprets_information,
                      rating_involves_others,
                      rating_decision_making_final_score)
write_csv(data123, "D:\\ML SIOP\\2023 ML SIOP\\Data\\Writing COde\\firstTry.csv")

## mimicking training data formatting - 3/24/23

validate <- read.csv("D:\\ML SIOP\\2023 ML SIOP\\Data\\Data2\\dev_pub.csv")

validate$predictor <- paste(validate$text_exercise_4,
                            validate$text_exercise_5,
                            validate$text_exercise_6,
                            validate$text_exercise_7,
                            validate$text_exercise_8,
                            validate$text_exercise_9,
                            validate$text_exercise_10,
                            validate$text_exercise_11,
                            validate$text_exercise_12,
                            validate$text_exercise_13,
                            validate$text_exercise_14,
                            validate$text_exercise_15,
                            validate$text_exercise_16,
                            validate$text_exercise_17,
                            validate$text_exercise_18,
                            validate$text_exercise_19,
                            validate$text_exercise_final)

library(stringr)
validate$predictor <- gsub('NA','',validate$predictor)

write.csv(validate[c(1,26)], "D:\\ML SIOP\\2023 ML SIOP\\Data\\Writing COde\\validate.csv")

## trying a lm prediction with simplified codings - 3/24/23

data123 <- read.csv("D:\\ML SIOP\\2023 ML SIOP\\Data\\Writing COde\\firstTry.csv")

# install.packages("quanteda")
# install.packages("quanteda.textstats")

library(quanteda)
library(quanteda.textstats)

data2 <- corpus(data123, docid_field = "predictor",
                text_field = "predictor")


tab2 <- textstat_readability(data2, measure = c("Flesch.Kincaid", "Dale.Chall"))

temp <- cbind(data123,tab2)

########################################
########################################


library(caret)

control <- trainControl(method="cv", number=10,verboseIter = TRUE)
metric <- "RMSE"

use1 <- temp[,c(10,11,2)]       ## ideally would just change the "." within ML scripts instead of this
df1 <- na.omit(use1)

use2 <- temp[,c(10,11,3)]
df2 <- na.omit(use2)

use3 <- temp[,c(10,11,4)]
df3 <- na.omit(use3)

use4 <- temp[,c(10,11,5)]
df4 <- na.omit(use4)

use5 <- temp[,c(10,11,6)]
df5 <- na.omit(use5)

use6 <- temp[,c(10,11,7)]
df6 <- na.omit(use6)

use7 <- temp[,c(10,11,8)]
df7 <- na.omit(use7)

set.seed(33)

fit.lm1 <- train(rating_chooses_appropriate_action ~ ., data=df1, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm2 <- train(rating_commits_to_action ~ ., data=df2, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm3 <- train(rating_gathers_information ~ ., data=df3, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm4 <- train(rating_identifies_issues_opportunities ~ ., data=df4, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm5 <- train(rating_interprets_information ~ ., data=df5, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm6 <- train(rating_involves_others ~ ., data=df6, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm7 <- train(rating_decision_making_final_score ~ ., data=df7, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

############################################
############################################

validate <- read.csv("D:\\ML SIOP\\2023 ML SIOP\\Data\\Writing COde\\validate.csv")[3]

data2 <- corpus(validate, docid_field = "predictor",
                text_field = "predictor")


tab2 <- textstat_readability(data2,measure = c("Flesch.Kincaid", "Dale.Chall"))
use2 <- tab2[-1]

rating_chooses_appropriate_action      <- predict(fit.lm1, use2)
rating_commits_to_action               <- predict(fit.lm1, use2)
rating_gathers_information             <- predict(fit.lm1, use2)
rating_identifies_issues_opportunities <- predict(fit.lm1, use2)
rating_interprets_information          <- predict(fit.lm1, use2)
rating_involves_others                 <- predict(fit.lm1, use2)
rating_decision_making_final_score     <- predict(fit.lm1, use2)

names <- read.csv("D:\\ML SIOP\\2023 ML SIOP\\Data\\Writing COde\\validate.csv")[2]

predictions2 <- cbind(names,rating_chooses_appropriate_action, rating_commits_to_action, rating_gathers_information, rating_identifies_issues_opportunities, rating_interprets_information, rating_involves_others, rating_decision_making_final_score)

write.csv(predictions2, "D:\\ML SIOP\\2023 ML SIOP\\Data\\Writing COde\\winner.csv")

## rescaling to see if it matters

predictions2$rating_chooses_appropriate_action <- scale(predictions2$rating_chooses_appropriate_action)*.9 + 2.5
predictions2$rating_commits_to_action <- scale(predictions2$rating_commits_to_action)*.9 + 2.5
predictions2$rating_gathers_information <- scale(predictions2$rating_gathers_information)*.9 + 2.5
predictions2$rating_identifies_issues_opportunities <- scale(predictions2$rating_identifies_issues_opportunities)*.9 + 2.5
predictions2$rating_interprets_information <- scale(predictions2$rating_interprets_information)*.9 + 2.5
predictions2$rating_involves_others <- scale(predictions2$rating_involves_others)*.9 + 2.5
predictions2$rating_decision_making_final_score <- scale(predictions2$rating_decision_making_final_score)*.9 + 4

write.csv(predictions2, "D:\\ML SIOP\\2023 ML SIOP\\Data\\Writing COde\\winner2.csv")