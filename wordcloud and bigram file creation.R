install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm")

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(SnowballC)

file <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021LIWCwithNGRAM.csv"
file <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021BalLIWC.csv"

data <- read.csv(file, stringsAsFactors=FALSE)

#Create a vector containing only the wanted text#
####All Sentences
text <- data$Text
####Yes only
text <- data[data$Tag == 'Yes', ]$Text
####No only
text <- data[data$Tag == 'No', ]$Text

# Create a corpus  
docs <- VCorpus(VectorSource(text))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords())
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)

#unigram
dtm <- TermDocumentMatrix(docs) 
dtm <- DocumentTermMatrix(docs) 


#bigram
biGrams <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
dtm <- TermDocumentMatrix(docs, control = list(tokenize = biGrams))

#trigram
triGrams <- function(x) {
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
dtm<- TermDocumentMatrix(docs, control = list(tokenize = triGrams))

#
matrix <- as.matrix(dtm) 
file <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/matrix.csv"
write.csv(matrix, file)

#

library(arkhe)

A <- IncidenceMatrix(data = sample(0:1, 100, TRUE, c(1, 1/3)), nrow = 20)
## Create a count data matrix
B <- CountMatrix(data = sample(0:10, 100, TRUE), nrow = 20)

A <- IncidenceMatrix(matrix)
## Create a count data matrix
B <- CountMatrix(matrix)

#WORDCLOUD2
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
require(devtools)
require(sourcetools)
install.packages("lchiffon/wordcloud2")
library(wordcloud2)
wordcloud2(data = df)
#

#separate bigrams to 2 columns for SNA
library(stringr)
df[c('word1', 'word2')] <- str_split_fixed(df$word, ' ', 2)
df <- df[c('word1', 'word2', 'freq')]
file <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021BigramAll.csv"
write.csv(df, file)
