#Use decision tree to quickly predict countries of choice
library(tree)
library(randomForest)
library(data.table)
library(corrplot)

data <- read.csv("train_processed.csv", header = T)
d <- data[,(2:25)]

d[is.na(d)] <- 0
d$age[d$age > 95] <- NaN
d$age[d$age < 13] <- NaN

#Decision Tree
model <- tree(country_destination~dummy_facebook+age+dummy_male+dummy_female, data=d)
plot(model)
text(model)
summary(model) # Misclassification error rate = 0.369

#Random Forest
model <- randomForest(country_destination~., data = d, nodesize = 15, sampsize = 0.3*nrow(d), replace = T, importance = T)
plot(model)
text(model)
summary(model)
importance(model)
barplot(model$importance[, 14], main = "Importance (Gini Index)")
barplot(model$importance[, 13], main = "Importance (Dec Accuracy)")

#Check Correlation
corr <- cor(d[,(1:14)])
corrplot(corr, method = "ellipse", type = "upper")

#Clustering to determine profiles of customers
#kmeans
d <- subset(d, select = c(1:14, 16:24))
#remove unselected features from random forest
d <- subset(d, select = c(1:2, 4:5, 15:16))
d[is.na(d)] <- 0
res <- kmeans(d, centers = 3, nstart = 5)
#pairs(d, col = c("red", "green", "blue")[res$cluster], pch = c(21,22,24)[res$cluster])
