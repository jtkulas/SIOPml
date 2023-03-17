library(tidyverse)

train <- read_csv("Boston2023/train_pub.csv")


# Sentiment analysis
library(tidytext)
get_sentiments

test <- train %>% unnest_tokens(word, text_exercise_4) %>% select(word)

# Bing
bing_pos <- get_sentiments("bing") %>% filter(sentiment == "positive")
bing_neg <- get_sentiments("bing") %>% filter(sentiment == "negative")

# Remove stop words
test <- test %>% anti_join(stop_words)

# positive
test %>% 
  inner_join(bing_pos) %>% 
  count(word, sort = T)




##### Paragraphs #####
test <- train %>% unnest_tokens(word, text_exercise_4, token = "paragraphs") %>% select(word)

test %>% filter(word %in% as.character(bing_pos))

# Count number of positive words in a paragraph
p <- 1
h <- 0
for (i in bing_pos) {
  if (str_detect(as.character(test[p,]), as.character(bing_pos[p,1]))) {
    h + 1
  }
  else{
    next
  }
  return(h)
}

