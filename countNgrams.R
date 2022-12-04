#retrieve n-grams
file1 <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/CDS N-Grams.csv"
ngrams <- read.csv(file1, stringsAsFactors=FALSE)

#library(stringr)
#ngramscombined <- data.frame(str_c(ngrams$Cognitive.Distortion, ' - ', ngrams$N.Grams))
#names(ngramscombined) <- c("ngrams")

#retrieve dataset
file2 <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021.csv"
sentences <- read.csv(file2, stringsAsFactors=FALSE)

#####INITIALIZATION OF NGRAM TOTAL COUNT############
for (i in 1:nrow(sentences)){ #for each sentence, add columns for cds n-grams count & initialize all values to zero
  for (j in 1:nrow(ngrams)){ #go through each ngram
    ngram <- ngrams[j,"N.Grams"] #get the value of the ngram
    sentences[i, ngram] <- 0 #set sentence ngram column to zero
  }
}
library(dplyr)
cd <- distinct(ngrams, ngrams$Cognitive.Distortion) #get the names of cognitive distortions/ cds ngrams categories
names(cd) <- ("cdcateg")
#initialize ngram count per category
for (i in 1:nrow(sentences)){
  for (j in 1:nrow(cd)){ #go through each ngram category
    ngramcd <- cd[j,"cdcateg"] #get the value of the ngram category
    sentences[i, ngramcd] <- 0 #set sentence ngram categ column to zero
  }
  sentences[i, "totalcds"] <- 0 #set total cds ngram to zero
}
#############################################
###############COUNT N-GRAMS#################
library(tokenizers)
library(stringr)
for (i in 1:nrow(sentences)){ #for each sentence

  for (j in 1:nrow(ngrams)){#for each ngram
    
    #check if a cds n-gram is found in the sentence
    if(str_detect(sentences[i, "Sentence"], ngrams[j,"N.Grams"])){
      
      ngramcolname <- ngrams[j,"N.Grams"] #get ngram value
      ngramcategname <- ngrams[j,"Cognitive.Distortion"] #get ngram categ/ cogdist
      sentences[i, ngramcolname] <- 1 #set column referring to ngram value as 1
      sentences[i, ngramcategname] <- sentences[i, ngramcategname] + 1 #update ngram category count
      sentences[i, "totalcds"] <- sentences[i, "totalcds"] + 1 #update totalcds
      
    }
  }
}
#save to file
file3 <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021-withNgramCount.csv"
write.csv(sentences, file3)

####################################
#######PROFILE SUMMARY##############
print(paste("Not Distorted, With CDS N-Gram: ", nrow(sentences[(sentences$Tag == 'No' & sentences$totalcds > 0), ])))
print(paste("Not Distorted, No CDS N-Gram:: ", nrow(sentences[(sentences$Tag == 'No' & sentences$totalcds == 0), ])))
print(paste("Distorted, With CDS N-Gram: ", nrow(sentences[(sentences$Tag == 'Yes' & sentences$totalcds > 0), ])))
print(paste("Distorted, No CDS N-Gram:: ", nrow(sentences[(sentences$Tag == 'Yes' & sentences$totalcds == 0), ])))
