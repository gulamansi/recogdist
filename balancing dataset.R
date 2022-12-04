
###################################
####BALANCING DATASET##############
#get only the sentences that are tagged as No
sentencesNo <- data.frame(sentences[sentences$Tag == 'No',])
#get random number same with Yes count
set.seed(12345)
sentencesNoRand <- sentencesNo[sample(1:nrow(sentencesNo), 3269), ]
#get only the sentences that are tagged as Yes
sentencesYes <- data.frame(sentences[sentences$Tag == 'Yes',])

#collate sentences YES and NO
sentences <- as.data.frame(rbind(sentencesNoRand, sentencesYes))


#write to file the balanced data set
file3 <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021Bal.csv"
write.csv(data_bal, file3)