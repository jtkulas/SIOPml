## mimicking training data formatting - 3/24/23

validate <- read.csv("Boston2023//dev_pub.csv")

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

write.csv(validate[c(1,26)], "Boston2023\\validate.csv")
