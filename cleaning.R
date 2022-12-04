##############################################
#CLEAN AND EXTRACT SENTENCES
fileloc <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/SR_covid19support_SEP-OCT.csv"
#read the file of extracted comments
dfn <- read.csv(fileloc, stringsAsFactors=FALSE)
#====Separate per month==#
#dfn <- dfn[format(as.Date(dfn$comm_date, format = "%d-%m-%Y"),"%m") == "05", ]
#========================#
dfn <- data.frame(dfn$X, dfn$comment)
names(dfn) <- c("com_id", "com_orig")

#Remove [DELETED] and [REMOVED] comments
dfn <- dfn[dfn$com_orig != "[deleted]" & dfn$com_orig != "[removed]", ]

#expand contraction
library(textclean)
dfn$com_orig <- replace_contraction(dfn$com_orig)

#Tokenize the comments into sentences.
library(tidytext)
library(dplyr)    # alternatively, this also loads %>%
dfn <- unnest_sentences(dfn, com_sentences, com_orig)
d <- dfn
dfn <- d %>% unnest_tokens(com_sentences, com_sentences, token = "regex", pattern = "[.]")

#Remove unwanted characters from the sentences.
library(tidyverse)
dfn$com_sentences <- gsub("[^[:alnum:][:blank:].?!&]", "", dfn$com_sentences)
dfn$com_sentences <- str_remove_all(dfn$com_sentences, "&ampquot")
dfn$com_sentences <- str_remove_all(dfn$com_sentences, "&ampx200b")
dfn$com_sentences <- str_remove_all(dfn$com_sentences, "&gt")
dfn$com_sentences <- str_remove_all(dfn$com_sentences, "&lt3")

#trim trailing spaces
dfn$com_sentences <- trimws(dfn$com_sentences)

#remove blanks
dfn <- dfn[!(is.na(dfn$com_sentences) | dfn$com_sentences ==""), ]

#remove sentences with 1 word
library(ngram)
for (i in 1:nrow(dfn)) {
  dfn[i,"wc"] <- wordcount(dfn[i, "com_sentences"])
}
dfn2 <- dfn
dfn2 <- dfn[dfn$wc > 1, ]

dfn2 <- data.frame(dfn2$com_id, dfn2$com_sentences)
names(dfn2) <- c("sentence_id", "sentences")
##############################################
#CONTRACTION REPLACEMENT MANUAL#
file2 <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021.csv"
df <- read.csv(file2, stringsAsFactors=FALSE)

library(stringr)
df$Sentence <- str_replace(df$Sentence, "^im ", "i am ")
df$Sentence <- str_replace(df$Sentence, " im ", " i am ")
df$Sentence <- str_replace(df$Sentence, "^ill ", "i will ")
df$Sentence <- str_replace(df$Sentence, " ill ", " i will ")
df$Sentence <- str_replace(df$Sentence, "^ive ", "i have ")
df$Sentence <- str_replace(df$Sentence, " ive ", " i have ")
df$Sentence <- str_replace(df$Sentence, "^iv ", "i have ")
df$Sentence <- str_replace(df$Sentence, " iv ", " i have ")
df$Sentence <- str_replace(df$Sentence, " hes ", " he is ")
df$Sentence <- str_replace(df$Sentence, "^hes ", " he is ")
df$Sentence <- str_replace(df$Sentence, " shes ", " she is ")
df$Sentence <- str_replace(df$Sentence, "^shes ", " she is ")
df$Sentence <- str_replace(df$Sentence, " shell ", " she will ")
df$Sentence <- str_replace(df$Sentence, "^shell ", " she will ")
df$Sentence <- str_replace(df$Sentence, "^youve ", "you have ")
df$Sentence <- str_replace(df$Sentence, " youve ", " you have ")
df$Sentence <- str_replace(df$Sentence, "^youd ", "you had ")
df$Sentence <- str_replace(df$Sentence, " youd ", " you had ")
df$Sentence <- str_replace(df$Sentence, "^youre ", "you are ")
df$Sentence <- str_replace(df$Sentence, " youre ", " you are ")
df$Sentence <- str_replace(df$Sentence, " its ", " it is ")
df$Sentence <- str_replace(df$Sentence, "^its ", "it is ")
df$Sentence <- str_replace(df$Sentence, " didnt ", " did not ")
df$Sentence <- str_replace(df$Sentence, " cant ", " can not ")
df$Sentence <- str_replace(df$Sentence, " wouldnt ", " would not ")
df$Sentence <- str_replace(df$Sentence, " shouldnt ", " should not ")
df$Sentence <- str_replace(df$Sentence, " doesnt ", " does not ")
df$Sentence <- str_replace(df$Sentence, " wont ", " will not ")
df$Sentence <- str_replace(df$Sentence, " wouldve ", " would have ")
df$Sentence <- str_replace(df$Sentence, " havent ", " have not ")
df$Sentence <- str_replace(df$Sentence, " hadnt ", " had not ")
df$Sentence <- str_replace(df$Sentence, " hasnt ", " had not ")
df$Sentence <- str_replace(df$Sentence, " dont ", " do not ")
df$Sentence <- str_replace(df$Sentence, "^dont ", " do not ")
df$Sentence <- str_replace(df$Sentence, " whats ", " what is ")
df$Sentence <- str_replace(df$Sentence, "^whats ", " what is ")
df$Sentence <- str_replace(df$Sentence, " idk ", " i do not know ")
df$Sentence <- str_replace(df$Sentence, "^idk ", "i do not know ")
df$Sentence <- str_replace(df$Sentence, " cuz ", " cause ")
df$Sentence <- str_replace(df$Sentence, " w ", " with ")
df$Sentence <- str_replace(df$Sentence, " hv ", " have ")
df$Sentence <- str_replace(df$Sentence, " rn ", " right now ")
df$Sentence <- str_replace(df$Sentence, " theres ", " there is ")
df$Sentence <- str_replace(df$Sentence, "^theres ", "there is ")
df$Sentence <- str_replace(df$Sentence, " isnt ", " is not ")
df$Sentence <- str_replace(df$Sentence, " theyll ", " they will ")
df$Sentence <- str_replace(df$Sentence, "^theyll ", "they will ")
df$Sentence <- str_replace(df$Sentence, " ok ", " okay ")
df$Sentence <- str_replace(df$Sentence, " u ", " you ")
df$Sentence <- str_replace(df$Sentence, " ur ", " your ")
df$Sentence <- str_replace(df$Sentence, " bc ", " because ")
df$Sentence <- str_replace(df$Sentence, " thx ", " thanks ")
df$Sentence <- str_replace(df$Sentence, " thats ", " that is ")
df$Sentence <- str_replace(df$Sentence, "^thats ", " that is ")
df$Sentence <- str_replace(df$Sentence, " werent ", " were not ")
df$Sentence <- str_replace(df$Sentence, "^werent ", " were not ")
df$Sentence <- str_remove_all(df$Sentence, "&amp")
df$Sentence <- str_remove_all(df$Sentence, "&gt")
df$Sentence <- str_remove_all(df$Sentence, "&lt3")
#save to file
fileloc2 <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021.csv"
write.csv(df, fileloc2)
#=========================================