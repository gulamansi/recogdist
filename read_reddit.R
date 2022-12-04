#####################
###  READ REDDIT ####
#####################
install.packages("RedditExtractoR")         
library(RedditExtractoR) 

#read text file containing subreddits to read
fileloc <-"C:/Users/user/Desktop/Text Mining Life/SUICIDE/SUICIDE 2022/subreddits.txt"
srlinks <- read.csv(fileloc, stringsAsFactors=FALSE, header = TRUE)

#read each subreddit and save contents to file
for (a in 1:nrow(srlinks)) {
  tryCatch({
    minedcontent <- find_thread_urls(subreddit=srlinks$subreddit_links[a], sort_by="new", period="all")
    fileloc <- paste("C:/Users/user/Desktop/Text Mining Life/SUICIDE/SUICIDE 2022/", srlinks$subreddit_links[a], "_raw.csv", sep="")
    write.csv(minedcontent, fileloc)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
#######################