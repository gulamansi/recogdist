#=========================================
#TOKENIZED SENTENCES INTO WORDS

#call the needed libraries for this task
library(tidytext)
library(dplyr)
library(tidyr)
library(stopwords)
stopwordsource <- "snowball"
#sources of stopwords: smart, snowball, stopwords-iso

#Retrieve the cleaned sentences and save into a new data frame.
fileloc2 <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021.csv"
dff <- read.csv(fileloc2, stringsAsFactors=FALSE)
dt <- data.frame(dff$X, dff$sentences)
names(dt) <- c("sentence_id","sentences")

#Remove stop words, tokenize by words, and save to data frame.
smc3 <- dt %>% unnest_tokens(word, sentences) %>% filter(!(word %in% stopwords(source = stopwordsource)))

#Set file location to store the word tokens.
sfl3 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/SR_covid19support_COMWORDS_ALL.csv"
sfl3 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/SR_covid19support_COMWORDS_August.csv"
sfl3 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/SR_covid19support_COMWORDS_July.csv"
sfl3 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/SR_covid19support_COMWORDS_June.csv"
sfl3 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/SR_covid19support_COMWORDS_May.csv"

#Save to file.
write.csv(smc3, sfl3)