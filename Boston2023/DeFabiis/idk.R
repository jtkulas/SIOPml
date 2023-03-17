library(tidyverse)

train <- read_csv("Boston2023/train_pub.csv")


# Sentiment analysis
library(tidytext)
get_sentiments

test <- train %>% unnest_tokens(word, text_exercise_4) %>% select(word)

# Bing
bing_pos <- get_sentiments("bing") %>% filter(sentiment == "positive")
bing_neg <- get_sentiments("bing") %>% filter(sentiment == "negative")

test <- test %>% anti_join(stop_words)

# positive
test %>% 
  inner_join(bing_pos) %>% 
  count(word, sort = T)



