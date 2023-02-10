#File Source
file1 <-"C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021LIWC-DTM-Bigram3.csv"
file1 <-"C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021-withNgramCount.csv"
file1 <-"C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021LIWC-SIGONLY-REDUCED2.csv"
file1 <-"C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021LIWC-DTM-Unigram3.csv"

#read data
my_data <- read.csv(file1, header = TRUE)

#assign row names or sentence numbers (each row = each sentence)
row.names(my_data) <- my_data$X

#assign classes (Yes/No)
colclass <- my_data$V33 #set the column representing the classes
categ <- as.factor(colclass)
classes <- levels(categ)
colors <- c("blue","red")

###Code Source:https://www.statology.org/multidimensional-scaling-in-r###

#compute distance matrix for each point/row/sentence using the unigrams as columns
d <- dist(my_data[,2:33]) #indicate the data columns
fit <- cmdscale(d, eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]

#plot points/rows/sentences
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Multidimensional Scaling Results", type="n")
text(x, y, labels=row.names(my_data),col = colors[factor(classes)])