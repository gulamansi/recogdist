library(superml)
library(e1071) 
library(tidyverse)
library(caret)

#load file
fl <- "C:/Users/Gulaman/Desktop/Text Mining Life/COGDIST/data1-dimmed-liwc.csv"
df <- read.csv(fl, stringsAsFactors=FALSE, header = TRUE)
summary(df)
#========================================
#see distribution of classes
y <- table(df$label)
View(y)
#========================================
#generate tfid feature
tfv <- TfIdfVectorizer$new(min_df = 0.001, remove_stopwords = TRUE, smooth_idf = TRUE)
tfv <- TfIdfVectorizer$new(min_df = 0.001, remove_stopwords = FALSE, smooth_idf = TRUE)
tfv <- TfIdfVectorizer$new(min_df = 0.01, remove_stopwords = FALSE, ngram_range=c(2,3), smooth_idf = TRUE)
tfv <- TfIdfVectorizer$new(min_df = 0.001, remove_stopwords = TRUE, ngram_range=c(3,3), smooth_idf = TRUE)

tf_mat <- tfv$fit_transform(df$sentence)

head(tf_mat)
#========================================
#FOR DTM
library(tm)
corpus <- VCorpus(VectorSource(df$sentence))
#clean corpus
corpus_clean <- tm_map(corpus, content_transformer(tolower))
#as.character(corpus_clean[[302]])
#as.character(corpus_clean[[10]])
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
library(SnowballC)
corpus_clean <- tm_map(corpus_clean, stemDocument)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
#as.character(corpus_clean[[302]])
#as.character(corpus_clean[[10]])
#generate dtm
library(tm)

dtm <- DocumentTermMatrix(corpus_clean)

dtm_tfidf <- DocumentTermMatrix(corpus_clean, control = list(weighting = weightTfIdf))
dtm_tfidf <- removeSparseTerms(dtm_tfidf, 0.95)

biGrams <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
bg_tdm <- DocumentTermMatrix(corpus_clean, control = list(tokenize = biGrams))

triGrams <- function(x) {
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
tg_tdm <- DocumentTermMatrix(corpus_clean, control = list(tokenize = triGrams))
#========================================
#FINALIZED DATASET
#shuffle, stratify, split train and test set
targets = as.factor(df$label)
#classes = levels(targets)
all_data <- as_tibble(cbind(as.matrix(dtm), target_ = targets))

all_data <- as_tibble(cbind(df$sscore, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm_tfidf), df$sscore, df$wc, df$nega, df$pronoun, df$health, df$verb, df$focusfuture, df$Analytic, target_ = targets))

all_data <- as_tibble(cbind(as.matrix(dtm), df$nega, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm), df$prosing, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm), df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm), df$sscore, df$nega, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm), df$sscore, df$prosing, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm), df$sscore, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm), df$nega, df$prosing, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm), sentiscore, df$nega, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm_tfidf), sentiscore, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm_tfidf), sentiscore, df$nega, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm_tfidf), df$wc, sentiscore, df$nega, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm_tfidf), df$wc, sentiscore, df$nega, df$pronoun, target_ = targets))
all_data <- as_tibble(df$pronoun, target_ = targets)
all_data <- as_tibble(df$wc, target_ = targets)
all_data <- as_tibble(df$sscore, df$wc, target_ = targets)
all_data <- as_tibble(df$sscore, df$nega, target_ = targets)
all_data <- as_tibble(df$sscore, df$pronoun, target_ = targets)
all_data <- as_tibble(df$sscore, df$wc, df$nega, df$pronoun, target_ = targets)
all_data <- as_tibble(cbind(as.matrix(dtm), df$sscore, df$nega, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm_tfidf), target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm), df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm_tfidf), df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, as.matrix(dtm_tfidf), df$sscore, df$prosing, df$nega, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, df$nega, df$prosing, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$prosing, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, df$prosing, df$nega, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, as.matrix(dtm), target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, df$prosing, df$nega, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, df$prosing, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, df$nega, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$nega, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$prosing, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, df$nega, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, df$prosing, target_ = targets))
all_data <- as_tibble(cbind(df$sscore, df$prosing, df$nega, target_ = targets))
all_data <- as_tibble(cbind(df$sscore, target_ = targets))
all_data <- as_tibble(cbind(df$sscore, df$nega, target_ = targets))
all_data <- as_tibble(cbind(df$sscore, df$prosing, target_ = targets))
all_data <- as_tibble(cbind(df$sscore, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(df$sscore, df$nega, df$prosing, target_ = targets))
all_data <- as_tibble(cbind(df$sscore, df$prosing, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))#all_data <- as_tibble(cbind(df$nega, target_ = targets))
all_data <- as_tibble(cbind(df$sscore, df$nega, df$prosing, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(df$sscore, df$nega, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(df$nega, df$prosing, target_ = targets))
all_data <- as_tibble(cbind(df$prosing, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$nega, df$prosing, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, df$nega, df$prosing, target_ = targets))
all_data <- as_tibble(cbind(df$prosing, df$nega, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(df$nega, df$prosing, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(df$nega, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(df$nega, target_ = targets))
all_data <- as_tibble(cbind(df$prosing, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$nega, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(df$hopeless, df$ac, df$pain, df$pb, df$tb, target_ = targets))
all_data <- as_tibble(cbind(tf_mat, df$sscore, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm_tfidf), df$wc, sentiscore, df$nega, df$pronoun, df$Analyctic, df$Clout, df$Authentic, df$function., df$i, df$shehe, df$ipron, df$verb,
                              df$interrog, df$posemo, df$insight, df$insight, df$discrep,
                         df$health, df$focuspresent, df$focusfuture,
                         df$certain, target_ = targets))
all_data <- as_tibble(cbind(df$Analytic, df$focusfuture, df$verb, as.matrix(dtm_tfidf), df$wc, df$sscore, df$nega, df$pronoun, df$health, target_ = targets))
all_data <- as_tibble(cbind(df$Analytic, df$focusfuture, df$verb, tf_mat, df$wc, sentiscore, df$nega, df$pronoun, df$health, target_ = targets))
all_data <- as_tibble(cbind(df$Analytic, df$focusfuture, df$verb, as.matrix(dtm), df$wc, sentiscore, df$nega, df$pronoun, df$health, target_ = targets))
all_data <- as_tibble(cbind(df$posemo , as.matrix(dtm_tfidf), df$Analytic, df$focusfuture, df$verb, df$wc, df$sscore, df$nega, df$pronoun, df$health, target_ = targets))
all_data <- as_tibble(cbind(df$wc, target_ = targets))
all_data <- as_tibble(cbind(as.matrix(dtm_tfidf), df$wc, df$sscore, df$nega, df$Analytic, df$Clout, df$Authentic,
                            df$function., df$pronoun, df$i, df$shehe, df$ipron, df$verb, df$interrog,
                            df$posemo, df$male, df$insight, df$discrep, df$certain, df$bio,
                            df$health, df$achieve, df$focusfuture, df$focuspresent, df$swear, target_ = targets))

#============================
#TRAIN AND TEST SET
#shuffle data to ensure classes are well-distributed
set.seed(42)
rows <- sample(nrow(all_data))
all_data <- all_data[rows, ]

train_set <- as_tibble(all_data) %>%
  group_by(target_)  %>% 
  sample_frac(size=.70) # "para sa train_set: 70% of samples per class"

test_set <- as_tibble(all_data) %>% anti_join(train_set)

#==================================
#CROSS_FOLD
#ctrlspecs <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)
# Define train control for k fold cross validation
#For Naive-Bayes
X <- subset(train_set, select=-target_)
Y <- as.factor(train_set$target_)
nb_model <- train(X, Y, method = "nb", trControl = trainControl(method = "cv", number = 10))
nrow(Y)
#For Linear SVM
ctrl <- trainControl(method = "cv", number=10, savePred=T, classProb=T)
svm_model <- train(target_~., data=train_set, method = "svmLinear", trControl = ctrl)
#==================================
#Naive-Bayes

nb_model <- naiveBayes(target_ ~ ., data = train_set)

#NB Model Performance Analysis
truth <- test_set$target_
pred <- predict(nb_model, test_set)
pred <- factor(pred, levels = c(1,2))
xtab <- table(pred, truth)
rownames(xtab) <- classes
colnames(xtab) <- classes
  prec <- precision(xtab)
  rec <- recall(xtab)
  F1 <- (2 * prec * rec) / (prec + rec)
  prec
  rec
  F1
cm <- confusionMatrix(xtab)
cm$overall['Accuracy']
  
#==================================
#SVM
#train SVM
svm_model = svm(target_ ~ ., data = train_set, kernel="linear")

#SVM Model Performance Analysis
truth <- test_set$target_

pred <- round(predict(svm_model, test_set))
#pred <- factor(pred, levels = c(1,2,3,4,5,6))
pred <- factor(pred, levels = c(1,2))
xtab <- table(pred, truth)
rownames(xtab) <- classes
colnames(xtab) <- classes
  prec <- precision(xtab)
  rec <- recall(xtab)
  F1 <- (2 * prec * rec) / (prec + rec)
  prec
  rec
  F1
cm <- confusionMatrix(xtab)
cm$overall['Accuracy']
