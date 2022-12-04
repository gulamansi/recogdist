
#Retrieve the cleaned comment sentences and save into a new data frame.
dfn <- read.csv(sfl2, stringsAsFactors=FALSE)
dt <- data_frame(dfn$X, dfn$com_sentences)
names(dt) <- c("sentence_id","sentences")

##############################################
#3 - BIGRAM
##############################################

#Tokenize original sentence into bigrams.
smc4 <- dt %>% unnest_tokens(bigram, sentences, token = "ngrams", n = 2)

#Separate the bigrams into two columns.
bigrams_separated <- smc4 %>% separate(bigram, c("word1", "word2"), sep = " ")

#Filter bigrams with stopwords.
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopwords(source = stopwordsource)) %>%
  filter(!word2 %in% stopwords(source = stopwordsource))
#  filter(!word1 %in% stop_words$word) %>%
#  filter(!word2 %in% stop_words$word)

#Save to data frame for uniformity.
smc4 <- bigrams_filtered

#Set file location to store the bigrams.
#sfl4 <- "C:/Users/Gulaman/Desktop/Text Mining Life/SUICIDE/SR_Depr_4_Bigrams.csv"
#sfl4 <- "C:/Users/Gulaman/Desktop/Text Mining Life/OFFMYCHEST/SR_Offm_4_Bigrams.csv"
#sfl4 <- "C:/Users/Gulaman/Desktop/Text Mining Life/SUICIDEPREV/SR_SuicidePrev_4_Bigrams.csv"
#sfl4 <- "C:/Users/Gulaman/Desktop/Text Mining Life/SUICIDEWATCH/SR_SuicideWatch_4_Bigrams.csv"
sfl4 <- "C:/Users/Gulaman/Desktop/Text Mining Life/THERAPY/SR_Therapy_4_Bigrams.csv"

#Save to file.
write.csv(smc4, sfl4)
##############################################
#4 - TRIGAM
##############################################

#Tokenize original sentence into trigrams.
smc5 <- dt %>% unnest_tokens(trigram, sentences, token = "ngrams", n = 3)

#Separate the trigrams into three columns.
trigrams_separated <- smc5 %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

#Filter trigrams with stopwords.
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stopwords(source = stopwordsource)) %>%
  filter(!word2 %in% stopwords(source = stopwordsource)) %>%
  filter(!word3 %in% stopwords(source = stopwordsource))

#Save to data frame for uniformity.
smc5 <- trigrams_filtered

#Set file location to store the bigrams.
#sfl5 <- "C:/Users/Gulaman/Desktop/Text Mining Life/SUICIDE/SR_Depr_5_Trigrams.csv"
#sfl5 <- "C:/Users/Gulaman/Desktop/Text Mining Life/OFFMYCHEST/SR_Offm_5_Trigrams.csv"
#sfl5 <- "C:/Users/Gulaman/Desktop/Text Mining Life/SUICIDEPREV/SR_SuicidePrev_5_Trigrams.csv"
#sfl5 <- "C:/Users/Gulaman/Desktop/Text Mining Life/SUICIDEWATCH/SR_SuicideWatch_5_Trigrams.csv"
sfl5 <- "C:/Users/Gulaman/Desktop/Text Mining Life/THERAPY/SR_Therapy_5_Trigrams.csv"

#Save to file.
write.csv(smc5, sfl5)
