
##################
#T-TEST via R
result <- t.test(data$verb[data$Tag=="Yes"], data$verb[data$Tag=="No"])
result$p.value

file <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021LIWC.csv"
data <- read.csv(file, stringsAsFactors=FALSE)

ttresult <- data.frame(matrix(ncol=6))
ttindex <- 1
colnames(ttresult) <- c("Attribute", "Mean of Yes", "Mean of No", "P-Value", "Significant?", "Status of YES")
for (i in 5:ncol(data)){
  tryCatch({
    result <- t.test(data[data[,"Tag"]=="Yes",i], data[data[,"Tag"]=="No",i])
    ttresult[ttindex, "Attribute"] <- colnames(data[i])
    ttresult[ttindex, "Mean of Yes"] <- result[["estimate"]][["mean of x"]]
    ttresult[ttindex, "Mean of No"] <- result[["estimate"]][["mean of y"]]
    ttresult[ttindex, "P-Value"] <- result[["p.value"]]
    if (result[["p.value"]] < 0.05)
      ttresult[ttindex, "Significant?"] <- "YES"
    else
      ttresult[ttindex, "Significant?"] <- "No"
    if ( result[["estimate"]][["mean of x"]] > result[["estimate"]][["mean of y"]])
      ttresult[ttindex, "Status of YES"] <- "Higher"
    else if ( result[["estimate"]][["mean of x"]] == result[["estimate"]][["mean of y"]])
      ttresult[ttindex, "Status of YES"] <- "Equal"
    else
      ttresult[ttindex, "Status of YES"] <- "Lower"
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  ttindex <- ttindex + 1
}
file <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/TTestResult-R.csv"
file <- "C:/Users/user/Desktop/Text Mining Life/COGDIST/September 2022/TTestResult-R-Bal.csv"

write.csv(ttresult, file)
#
