# Text classification script for sentiment analysis
#load libraries
library("e1071")
library("tm")
library("RTextTools")

#function to read the text files (separated by #######)
readText <- function(filename){
  result<- c()
  current.line <- 1
  con <- file(filename)
  open(con)
  tmp = ""
  while(length(line <- readLines(con, n=1, warn=FALSE)) > 0 ){
    if (current.line == 1){
      tmp = line
    }
    else{
      if (line == "#######"){
        result <- c(result, tmp)
        tmp = ""
      }
      else{
        tmp <- paste(tmp, line)
      }
    }
    current.line <- current.line + 1
  }
  
  close(con)
  return (result)
}

#predict test function
predictTest <- function(test_text, mat, classifier){
  train_mat = mat[1:2,]
  train_mat[,1:ncol(train_mat)] = 0
  
  test_matrix = create_matrix(test_text, language="english", removeStopwords=T, removeNumbers=T,stemWords=T, toLower=T, removePunctuation=T)
  test_mat <- as.matrix(test_matrix)
  
  for(col in colnames(test_mat)){
    if(col %in% colnames(train_mat))
    {
      train_mat[2,col] = test_mat[1,col];
    }
  }
  
  #test_mat = as.matrix(t(test_mat))
  row.names(train_mat)[1] = ""
  row.names(train_mat)[2] = test_text
  p <- predict(classifier, train_mat[1:2,])
  as.character(p[2])
}

#read a corpus of positive tweets
pos <- tolower(readText("positive.txt"))
pos_class <- rep("positive", length(pos))
pos_data <- cbind(pos, pos_class)

#read a corpus of negative tweets
neg <- tolower(readText("negative.txt"))
neg_class <- rep("negative", length(neg))
neg_data <- cbind(neg, neg_class)

training_data <- rbind(pos_data[1:20000,], neg_data[1:20000,])

#ignore the class column
matrix <- create_matrix(training_data[,1], language="english", removeStopwords=T, removeNumbers=T,stemWords=T, toLower=F, removePunctuation=T, removeSparseTerms=0.998)

mat = as.matrix(matrix)
nb_classifier = naiveBayes(mat, as.factor(training_data[,2]))

#for testing & debugging
predictTest("wow. that's nice!", mat, nb_classifier)
predictTest("I hate it", mat, nb_classifier)

#read in cleaned airbnb tweets
tweets <- read.csv("compiled_tweets.csv", header = T)
tweets$tweet <- sapply(tweets$tweet,function(row) iconv(row, "latin1", "ASCII", sub=""))
results <- seq(from=1, to=nrow(tweets))
n <- nrow(tweets)
#for each tweet, predictTest
for (i in 1:n){
  tweet <- tolower(tweets[i,5])
  r <- try(predictTest(tweet, mat, nb_classifier))
  #if (inherits(r, "try-error")) { continue }
  results[i] = r
  print(i/n)
}

results <- cbind(tweets, results)
write.csv(results, file = "sentiment.csv", row.names = F)
  