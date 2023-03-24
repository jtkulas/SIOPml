train <- read.csv("D:\\ML SIOP\\2023 ML SIOP\\Data\\train_pub.csv")
library(descr)

#Using the tutorial for NLP and appling it to the data set

library(qdap)
frequent_terms2023ML <- freq_terms(train,10)

#plot
plot(frequent_terms2023ML)

#/
library(readr)
datasetNLP <- read.csv("D:\\ML SIOP\\2023 ML SIOP\\Data\\train_pub.csv")

str(datasetNLP)
nrow(datasetNLP)

#isolating text from data
sentences_textE4 <- datasetNLP$text_exercise_4 #Text Exercise 4
str(sentences_textE4)

#Building A Corpus
library(tm)
S_tE5 <- VectorSource(datasetNLP$text_exercise_5) #Sentences as Vector Text Exercise 5

#Volatile Corpus: Volatile E5
sentences_textE5 <- VCorpus(S_tE5)
sentences_textE5

#Print out row 2
sentences_textE5[[2]]
#Print out content of Exercise 5, row 2
sentences_textE5[[2]][1]
#The string of Exercise 5, row 2
str(sentences_textE5[[2]])

#creating text dataframe, using Exercise 4,5,6
#for tm package Dataset2
#(num = c(1,2,3), doc_id = c("Text mining is a great time.", "Text analysis provides insights", "qdap and tm are used in text mining"), text = c("R is a great language", "R has many uses", "R is cool!"), stringsAsFactors = FALSE)
Dataset2 <- read.csv("D:\\ML SIOP\\2023 ML SIOP\\Data\\train_pub.csv", stringsAsFactors = F)
tE4E5E6 <- data.frame()
textE4E5E6 <- data.frame(num = c(1,2,3), doc_id = row.names(Dataset2), text= c(datasetNLP$text_exercise_4, datasetNLP$text_exercise_5, datasetNLP$text_exercise_6))
textE4E5E6
df_sourcetNLP <- DataframeSource(textE4E5E6[, 2:3])
df_CorpustNLP <- VCorpus(df_sourcetNLP)
#examining df_CorpustNLP
df_CorpustNLP
#ye bby
str(df_CorpustNLP)
#
text1 <- "<b> He </b> types at    his computer from 9-5 P.M. , he likes to type. What is he typing? probably a computer."
tolower(text1)
#removePunctuation
removePunctuation(text1)
##
stripWhitespace(text1)
bracketX(text1)
#
replace_number(text1)
#
replace_abbreviation(text1)
replace_contraction(text1)
replace_symbol(text1)
#stop words
stopwords("en")
removeWords(text1, stopwords("en"))
#SIOP 2023 NLP stopwords "the", "to"
S23NLPnew_stops <- c("the", "to")

removeWords(text1, S23NLPnew_stops)
#creating stems
stemDocument(c("motivated", "motivation", "motivational"))
#positive
motivation_words <- c("motivated", "motivation", "motivational")
#stem_doc
stem_doc <- stemDocument(motivation_words)
#creating list or "dictionary" of positive words
motivate_dict <- ("motivate")
#stem completion: complete_text_E4-E6
complete_E4toE6 <- stemCompletion(stem_doc,motivate_dict)
#print C_E4tE6, motivate
complete_E4toE6

#Creating a stemDocument
stemDocument("I am motivated, I am motivating, I am motivation.")

text_data1 <- "I am motivated, I am motivating, I am motivation."
rm_punch1 <- removePunctuation(text_data1)
nchar_vec1 <- unlist(strsplit(rm_punch1, split = ' '))
# Word stem: stem_doc1
stem_doc1 <- stemDocument(nchar_vec1)
#stemdoc1
stem_doc1
#creating the motivate dictionary: motiv_dict
motiv_dict <- c("I", "am", "motiv")
#complete stem doc 2: complete_motiv_doc
complete_motiv_doc <- stemCompletion(stem_doc1, motiv_dict)
complete_motiv_doc

#cleaning the corpus, "tm_map()" maps funcitons to the entire corpus
#clean_corpus(), context_transformer(), removeNumbers(), replace_number()
frequent_termsE4E5E6EZ <- freq_terms(textE4E5E6)
str(frequent_termsE4E5E6EZ)
#Building da Corpus
library(tm)
text_sourceE4toE6 <- VectorSource(textE4E5E6)
#creating volatile corpus: text_corpusE4t6
text_corpusE4t6 <- VCorpus(text_sourceE4toE6)
text_corpusE4t6
#print row 2 of E4 to E6
text_corpusE4t6[[2]]
#print E4 to E6 corpus
text_corpusE4t6[[3]][1]
str(text_corpusE4t6[3])
#creating corpus dataframe
data_textE4E5E6_vCorpus <- data.frame(num = c(1,2,3), doc_id = row.names(Dataset2), text= c(datasetNLP$text_exercise_4, datasetNLP$text_exercise_5, datasetNLP$text_exercise_6))
df_source2 <- DataframeSource(data_textE4E5E6_vCorpus[,2:3])
df_corpus2 <- VCorpus(df_source2)
df_corpus2
str(df_corpus2)
#the cleaning process "text"
texty2 <- "What <b> is a B?      I need to know 5 things about  what it means to be human!! He cannot explain, but the difference is 60% water and the rest unknown.."
#lower
tolower(texty2)
removePunctuation(texty2)
stripWhitespace(texty2)
#
bracketX(texty2)
replace_number(texty2)
replace_abbreviation(texty2)
replace_contraction(texty2)
replace_symbol(texty2)

#Using STop words

train_Boston_stops <- c("the", "to")
removeWords(texty2, train_Boston_stops)

#stem completion using word stems
stemDocument(c("motivated", "motivation","motivating"))
motivating_sentence <- "In a motivated haste, Ian rushed to fix a new motivation, too motivating."
remove_punch <- removePunctuation(motivating_sentence)
#Creating char vector: naught_char_vec
naught_char_vec <- unlist(strsplit(remove_punch, split = ' '))
##
stemmed_document <- stemDocument(naught_char_vec)
stemmed_document #creating the stem for the doc, then the complete dictionary
completed_dictionary <- c("In", "a", "motivate", "haste", "Ian", "rush", "to", "fix", "new", "too")
complete_doc <- stemCompletion(stemmed_document, completed_dictionary)
complete_doc
### Frequent terms
frequent_terms_Boston <- freq_terms(textE4E5E6, 30)
plot(frequent_terms_Boston)

#Cleaning up the corpus of Boston
clean_corpus_boston <- function(df_corpus2){
  df_corpus2 <- tm_map(df_corpus2, stripWhitespace)
  df_corpus2 <- tm_map(df_corpus2, removePunctuation)
  df_corpus2 <- tm_map(df_corpus2, content_transformer(tolower))
  df_corpus2 <- tm_map(df_corpus2, removeWords, stopwords("en"))
  return(df_corpus2)
}
#Applying Fabulso customized function to
clean_corp_Rivera <- clean_corpus_boston(sentences_textE5)
#Print out cleaned answer from E5
clean_corp_Rivera
clean_corp_Rivera[[900]][2]
clean_corp_Rivera$content
clean_corp_Rivera[[1459]][1]
# creating DTM of from Boston Corpus:
ML2023Boston_DTM_E5 <- DocumentTermMatrix(clean_corp_Rivera)
ML2023Boston_DTM_E5

#Creating the Matrices, E5
ColumnE5_m <- as.matrix(ML2023Boston_DTM_E5)
dim(ColumnE5_m)
ColumnE5_m[1253:1300, 2500:2600]
ColumnE5_m[1253:1256, 2500:2503]
#removing low freq. terms
E5_dtm_rm_sparsity <- removeSparseTerms(ML2023Boston_DTM_E5, 0.98)
E5_dtm_rm_sparsity

Matrix_Boston_E5 <- as.matrix(E5_dtm_rm_sparsity)
# Matrix_Boston_E5 <- cbin("Chooses")
dim(Matrix_Boston_E5)
Matrix_Boston_E5[1350:1375, 74:79] # 2 "think"(1371), 1 "time"(1363)
#Love Ian.

#creating dtm for the participants "ID"
#participant_ID_dtm <- DocumentTermMatrix()
#rotating the matrix
Rubix_Boston_Participants_ID <- t(Matrix_Boston_E5)
Rubix_Boston_Participants_ID

#doing the remove sparsity for rotated Boston

library(tm)
Boston_Matrix_2023_23_3 <- VectorSource(Rubix_Boston_Participants_ID)
Boston_Matrix_2023_23_3
BM23233_corpus <- VCorpus(Boston_Matrix_2023_23_3)
BM23233_corpus
BM23233_corpus[[37]]
BM23233_corpus[[6]][1] #found "can" for participant 1

## applying ML
set.seed(9001)
Boston_Matrix_2023_23_3
str(Boston_Matrix_2023_23_3)
class(Boston_Matrix_2023_23_3)
rows <- sample(Boston_Matrix_2023_23_3)
class(rows)
str(rows)
shuffled_words <- Boston_Matrix_2023_23_3[rows,]
shuffled_words

WwF <- as.data.frame(shuffled_words)
WwF
str(WwF)
str(shuffled_words)
class(shuffled_words)
##
split <- Boston_Matrix_2023_23_3[,1:1173]
split
#creating training set
train <- Boston_Matrix_2023_23_3[,1:1173]
trainer2 <- as.data.frame(shuffled_words[,1:1173])
class(trainer2)
#test set
#test <-
model <- lm(WwF$`1` ~ ., trainer2)
plot(model)
#barplot(table(model$coefficients))
#predicting
p <- predict(model, trainer2)
plot(p)
# data("crude")
# crude <- as.VCorpus(Rubix_Boston_Participants_ID)
# crude <- tm_map(crude, stripWhitespace)
# crude <- tm_map(crude, removePunctuation)
# crude <- tm_map(crude, content_transformer(tolower))
# crude <- tm_map(crude, removeWords, stopwords("english"))
# crude <- tm_map(crude, stemDocument)
# dtm <- DocumentTermMatrix(crude)
# sdtm <- removeSparseTerms(dtm, 0.3)
# sdtm2 <- removeSparseTerms(dtm, 0.7)
#
# sdtm$ncol
# inspect(sdtm) # 4 words returned
# sdtm2$ncol
# inspect(sdtm2) # 24 words returned

Rubix_boston_Participants_id <- removeSparseTerms(Rubix_Boston_Participants_ID, 0.98)


#For ID , sparsity for People, computing rows
# associating 2 with row(1371)
#give case"row" 1371 the value = 2


#trying to add columns for the matrix
#datasetNLP$Decision_Maker <- c("Chooses", "appropriate", "action")
#Matrix_Boston_E5[1350:1375, Decision_Maker]


#install.packages("autokeras")
library(autokeras)

#
#
# CCdS23NLP <- removeWords(df_CorpustNLP, S23NLPnew_stops)

#tolower ALl lowercase

#lowerCaseE4to6 <- tolower(df_CorpustNLP)
#print(lowerCaseE4to6)