############correlation#############
#cor(data$WC, data$Analytic)
correl <- data.frame()
#header
correl[1,1] <- "X"
x <- 2
for (j in 6:ncol(data)){
  correl[1,x] <- colnames(data[j])
  x <- x + 1
}
#create correlation matrix
r <- 2
for (i in 6:ncol(data)){
  correl[r,1] <- colnames(data[i]) #first column name for each row  
  c <- 2
  for (j in 6:ncol(data)){
    tryCatch({
      correl[r,c] <- cor(data[,i], data[,j])
      #cor(data[,i], data[,j])
      #print(paste(i,j,sep = "-")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    c <- c+1
  }
  r <- r+1
}
file <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Correlation.csv"
write.csv(correl, file)
# save high correlation info
correlhigh <- data.frame(matrix(ncol=3))
colnames(correlhigh) <- c("Attrib1", "Attrib2", "Correlation")
corindex <- 1
for (i in 2:ncol(correl)){
  ii <- i+1
  for (j in ii:ncol(correl)){
    tryCatch({
      if (abs(as.double(correl[i,j])) >= 0.7 & i != j){
        correlhigh[corindex, "Attrib1"] <- correl[i,1]
        correlhigh[corindex, "Attrib2"] <- correl[1,j]
        correlhigh[corindex, "Correlation"] <- correl[i,j]
        corindex <- corindex + 1
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}