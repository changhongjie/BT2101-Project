library(tm)
library(wordcloud)
library(RColorBrewer)
library(stringr)

#read in tweets and bind
tweets1 <- read.csv("airbnb_twitter_27sep.csv", header = T)
tweets2 <- read.csv("airbnb_twitter_28sep.csv", header = T)
tweets3 <- read.csv("airbnb_twitter_29sep.csv", header = T)
tweets4 <- read.csv("airbnb_twitter_30sep.csv", header = T)
tweets5 <- read.csv("airbnb_twitter_01oct.csv", header = T)
tweets6 <- read.csv("airbnb_twitter_02oct.csv", header = T)
tweets7 <- read.csv("airbnb_twitter_03oct.csv", header = T)
tweets8 <- read.csv("airbnb_twitter_04oct.csv", header = T)
tweets9 <- read.csv("airbnb_twitter_05oct.csv", header = T)
tweets10 <- read.csv("airbnb_twitter_06oct.csv", header = T)
tweets11 <- read.csv("airbnb_twitter_07oct.csv", header = T)
tweets12 <- read.csv("airbnb_twitter_08oct.csv", header = T)
tweets13 <- read.csv("airbnb_twitter_09oct.csv", header = T)
tweets14 <- read.csv("airbnb_twitter_10oct.csv", header = T)
tweets <- rbind(tweets1, tweets2, tweets3, tweets4, tweets5, tweets6, tweets7)
tweets <- rbind(tweets8, tweets9, tweets10, tweets11, tweets12, tweets13, tweets14)

fileName <- "airbnb_brand.txt"
airbnbText <- readChar(fileName, file.info(fileName)$size)

fileName <- "compiled_tweets.csv"
f <- read.csv(fileName, header = T)
f$tweet <- sapply(f$tweet,function(row) iconv(row, "latin1", "ASCII", sub=""))
f1 <- f[1:130000,]
f2 <- f[130001:nrow(f),]
newsText <- read.csv("allnews.csv", header = T)

corpus <- Corpus(VectorSource(f$tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
# remove stopwords
# keep "r" by removing it from stopwords
myStopwords <- c(stopwords('english'), "available", "via", "facebook", "twitter", "share", "image", "caption", "copyright", 
                 "pinterest", "linkedin", "whatsapp", "email", "messenger", "says", "RT", "airbnb", "new", "york",
                 "airbnbhttpairbnbblogcom")
stop <- stopwords('english')

idx <- which(myStopwords == "r")
myStopwords <- myStopwords[-idx]
corpus <- tm_map(corpus, removeWords, myStopwords)
dictCorpus <- corpus
corpus <- tm_map(corpus, stemDocument, lazy = T)
corpus <- tm_map(corpus, stemCompletion, dictionary=dictCorpus, lazy = T)

tdm <- TermDocumentMatrix(corpus, control = list(minWordLength = 1))
tdm <- TermDocumentMatrix(corpus, control =
                            list(removePunctuation = TRUE, stemming = F,
                                 stopwords = stop, removeNumbers = TRUE, tolower = TRUE))
tdm <-removeSparseTerms(tdm, 0.99999)

freq <- findFreqTerms(tdm, 1100)
dm = data.frame(value = tm_term_score(tdm, freq, FUN = slam::row_sums))
#dm = dm[order(dm$value)]
m = as.matrix(dm)
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs)
wordcloud(dm$word, dm$freq, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))

# alternative
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs)
wordcloud(dm$word, dm$freq, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"), max.words = 50)

x <- findAssocs(tdm, "airbnb", 0.02)
