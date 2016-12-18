bbc <- read.csv("bbc_ex 19th Oct 15_30.csv", header = T)
cnn <- read.csv("cnn_ex 19th Oct 01_59.csv", header = T)
nyt <- read.csv("nyt_ex 19th Oct 16_00.csv", header = T)
total <- rbind(bbc, cnn, nyt)

total <- read.csv("allnews.csv", header = T)
