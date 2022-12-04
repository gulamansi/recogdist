#file to read texts from
flc1 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/May-August 2021/SR_covid19support_ALL-POSTS.csv"
flc1 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/Sep-October 2021/SR_covid19support_COMSENTENCES_SEP-OCT.csv"

#file to save the texts with sscore
flc2 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/May-August 2021/SR_covid19support_ALL-POSTS-withSS.csv"
flc2 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/Sep-October 2021/SR_covid19support_COMSENTENCES_SEP-OCT-withSS.csv"

##############################################
#1 - GET SENTIMENT SCORE PER COMMENT SENTENCE (ADD TO NEW COLUMN)

#read the file of extracted comments
dfn <- read.csv(flc1, stringsAsFactors=FALSE)
dfn <- data.frame(dfn$X, dfn$sentences)
names(dfn) <- c("sentence_id", "sentences")

library(sentimentr) #library needed

#get score per sentence
for (i in 1:nrow(dfn)) {
  dfn[i,"sscore"] <- sentiment_by(dfn[i, "sentences"], by=NULL)$ave_sentiment
  print(paste(i, " ", dfn[i,"sscore"]))
}

write.csv(dfn, flc2) #save to file
##############################################

##############################################
#2 - GET ONLY THE SENTENCES WITH NEGATIVE SENTIMENTS

#read the file with sscore
flc2 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/May-August 2021/SR_covid19support_COMSENTENCES_ALL-withSS.csv"
flc3 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/May-August 2021/SR_covid19support_COMSENTENCES_ALL-withSS-NEGA.csv"
dfn2 <- read.csv(flc2, stringsAsFactors=FALSE)
dfn2 <- data.frame(dfn2$X, dfn2$sentences, dfn2$sscore)
names(dfn2) <- c("sentence_id", "sentences", "sscore")

dfn2 <- dfn2[dfn2$sscore < 0, ] #get negatives only

#SORT FROM LOWEST TO HIGHEST
dfn2 <- dfn2[order(dfn2$sscore),]
write.csv(dfn2, flc3) #save to file

#GET Top 501 to ...
dfn2 <- dfn2[501:5000,]
flc4 <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/May-August 2021/SR_covid19support_COMSENTENCES_ALL-withSS-NEGA501to5000.csv"
write.csv(dfn2, flc4) #save to file

##############################################
#SORT ACCORDING TO SENTIMENT SCORE
#read the file with sscore
fl <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/May-August 2021/SR_covid19support_ALL-POSTS-withSS.csv"
fl <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/May-August 2021/SR_covid19support_COMSENTENCES_ALL-withSS.csv"
fl <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/Sep-October 2021/SR_covid19support_POST_SEP-OCT-withSS.csv"
fl <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/Sep-October 2021/SR_covid19support_COMSENTENCES_SEP-OCT-withSS.csv"

dfn2 <- read.csv(fl, stringsAsFactors=FALSE)
dfn2 <- data.frame(dfn2$X, dfn2$sentences, dfn2$sscore)
names(dfn2) <- c("sentence_id", "sentences", "sscore")

#get 500 most negative sentiment
sdfn2 <- dfn2[order(dfn2$sscore),]
sdfn2 <- sdfn2[1:500,]

#add to FIRST 2000 data set
#create data frame for the 500
sdfn2$period <- "may-aug2021"
sdfn2$source <- "post"
fl2k <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/First2k-MostNegaSenti.csv"
#For first values
write.csv(sdfn2, fl2k) #save to file
#succeeding values
write.table(sdfn2, fl2k, append=TRUE, col.names=FALSE, sep = ",", quote = FALSE) #save to file

##############################################
#GET RANDOM 500
#read the file with sscore
fl <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/May-August 2021/SR_covid19support_ALL-POSTS-withSS.csv"
fl <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/May-August 2021/SR_covid19support_COMSENTENCES_ALL-withSS.csv"
fl <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/Sep-October 2021/SR_covid19support_POST_SEP-OCT-withSS.csv"
fl <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/Sep-October 2021/SR_covid19support_COMSENTENCES_SEP-OCT-withSS.csv"

dfn2 <- read.csv(fl, stringsAsFactors=FALSE)
dfn2 <- data.frame(dfn2$X, dfn2$sentences, dfn2$sscore)
names(dfn2) <- c("sentence_id", "sentences", "sscore")

#from the total number of sentences, extract 500 samples randomly
rand500 <- sample(dfn2$sentence_id, 500)
for (i in 1:nrow(dfn2)) {
  ifelse(dfn2[i, "sentence_id"] %in% rand500, dfn2[i,"isInFirst500"] <- "YES", dfn2[i,"isInFirst500"] <- "NO")
}
y <- table(dfn2$isInFirst500)
View(y)
#update the file
write.csv(dfn2, fl) #save to file

#add to FIRST 2000 data set
#create data frame for the 500
dfn3 <- dfn2[dfn2$isInFirst500 == "YES",]
dfn3$period <- "sep-oct2021"
dfn3$source <- "comments"
fl2k <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/First2k.csv"
#For first values
write.csv(dfn3, fl2k) #save to file
#succeeding values
write.table(dfn3, fl2k, append=TRUE, col.names=FALSE, sep = ",", quote = FALSE) #save to file
