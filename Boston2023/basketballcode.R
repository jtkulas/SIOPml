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

use <- temp[,c(10,11,2)]
df <- na.omit(use)

set.seed(32)

fit.lm <- train(rating_chooses_appropriate_action ~ ., data=df, method="lm", metric=metric, trControl=control, preProcess = "knnImpute")

fit.wm <- train(rating_chooses_appropriate_action ~ ., data=df, method="WM", metric=metric, trControl=control, preProcess = "knnImpute")
# fit.evtree <- train(POSTSEASON ~ ., data=use2, method="evtree", metric=metric, trControl=control)

# CART
fit.cart <- train(rating_chooses_appropriate_action ~ ., data=df, method="rpart", metric=metric, trControl=control, preProcess = "knnImpute")

# kNN
fit.knn <- train(rating_chooses_appropriate_action ~ ., data=df, method="knn", metric=metric, trControl=control, preProcess = "knnImpute")
# c) advanced algorithms
# SVM
fit.svm <- train(rating_chooses_appropriate_action ~ ., data=df, method="svmRadial", metric=metric, trControl=control, preProcess = "knnImpute")
# Random Forest
fit.rf <- train(rating_chooses_appropriate_action ~ ., data=df, method="rf", metric=metric, trControl=control, preProcess = "knnImpute")


# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf, lm=fit.lm, wm=fit.wm))
summary(results)
dotplot(results)


validate <- read.csv("Boston2023\\validate.csv")[3]

data2 <- corpus(validate, docid_field = "predictor",
                text_field = "predictor")


tab2 <- textstat_readability(data2,
                             measure = c("Flesch.Kincaid", "Dale.Chall"))
use2 <- tab2[-1]

rating_chooses_appropriate_action <- predict(fit.lm, use2)

names <- read.csv("Boston2023\\validate.csv")[2]

predictions2 <- cbind(names,rating_chooses_appropriate_action)
write.csv(predictions2, "Boston2023//winner.csv")


