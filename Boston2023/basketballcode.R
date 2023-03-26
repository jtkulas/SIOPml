## trying a lm prediction with simplified codings - 3/24/23

data <- read.csv("Boston2023/firstTry.csv")

library(quanteda)
library(quanteda.textstats)

data2 <- corpus(data, docid_field = "predictor",
                text_field = "predictor")


tab2 <- textstat_readability(data2,
                             measure = c("Flesch.Kincaid", "Dale.Chall"))

temp <- cbind(data,tab2)

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

set.seed(32)

fit.lm1 <- train(rating_chooses_appropriate_action ~ ., data=df1, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm2 <- train(rating_commits_to_action ~ ., data=df2, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm3 <- train(rating_gathers_information ~ ., data=df3, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm4 <- train(rating_identifies_issues_opportunities ~ ., data=df4, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm5 <- train(rating_interprets_information ~ ., data=df5, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm6 <- train(rating_involves_others ~ ., data=df6, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.lm7 <- train(rating_decision_making_final_score ~ ., data=df7, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

############################################
############################################

validate <- read.csv("Boston2023\\validate.csv")[3]

data2 <- corpus(validate, docid_field = "predictor",
                text_field = "predictor")


tab2 <- textstat_readability(data2,
                             measure = c("Flesch.Kincaid", "Dale.Chall"))
use2 <- tab2[-1]

rating_chooses_appropriate_action      <- predict(fit.lm1, use2)
rating_commits_to_action               <- predict(fit.lm1, use2)
rating_gathers_information             <- predict(fit.lm1, use2)
rating_identifies_issues_opportunities <- predict(fit.lm1, use2)
rating_interprets_information          <- predict(fit.lm1, use2)
rating_involves_others                 <- predict(fit.lm1, use2)
rating_decision_making_final_score     <- predict(fit.lm1, use2)

names <- read.csv("Boston2023\\validate.csv")[2]

predictions2 <- cbind(names,rating_chooses_appropriate_action, rating_commits_to_action, rating_gathers_information, rating_identifies_issues_opportunities, rating_interprets_information, rating_involves_others, rating_decision_making_final_score)

write.csv(predictions2, "Boston2023//winner.csv")

descr::freq(predictions2$rating_chooses_appropriate_action)  ## need to expand to 1-->4, empirically 2.4 --> 3.227


