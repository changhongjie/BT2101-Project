# merge sentiment data for 2 weeks and plot bar chart
library(scales)
library(ggplot2)

filenames = c("30sep_test_sentiment.csv", "29sep_test_sentiment.csv", "28sep_test_sentiment.csv", "27sep_test_sentiment.csv",
              "01oct_test_sentiment.csv", "02oct_test_sentiment.csv", "03oct_test_sentiment.csv", "04oct_test_sentiment.csv",
              "05oct_test_sentiment.csv", "06oct_test_sentiment.csv", "07oct_test_sentiment.csv", "08oct_test_sentiment.csv",
              "09oct_test_sentiment.csv", "10oct_test_sentiment.csv")
tweets <- lapply(filenames, read.csv, header = T)
t <- read.csv("airbnbxxx.csv", header = T)
tweets <- do.call(rbind, tweets)
df <- data.frame(tweets)
write.csv(df, file = "airbnbxxx.csv", row.names = F)
results <- df$results
barplot(height = table(t$results), beside = T,  ylab = "counts of tweets")
p <- ggplot(t, aes(factor(t$results))) + geom_bar(aes(y = (..count..)/sum(..count..), fill = t$results)) + scale_y_continuous(labels = percent)
p <- p + xlab("Tweet Categories") + ylab("Percentage of Total Tweets") + theme_bw() + scale_color_hue()
p
