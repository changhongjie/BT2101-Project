library(caret)
library(e1071)
library(rpart)

#Preprocess data used for profiling hosts
files <- list.files(pattern = ".csv")
listings <- do.call(rbind,lapply(files,read.csv, header =T, na.strings =""))

files <- list.files(pattern = ".csv")
time <- do.call(rbind,lapply(files,read.csv, header =T, na.strings =""))

df <- subset(listings, select = c(1:1, 7:12, 14:16))
df <- na.omit(df)

dummy_features <- "room_type"
dummy_rooms <- dummyVars(~room_type, data = df)
df_all <- as.data.frame(predict(dummy_rooms, newdata = df))
df_combined <- cbind(df[,-c(which(colnames(df) == "room_type"))], df_all)

#Process scoring of reviews per availability
df_combined$score <- df_combined$number_of_reviews / df_combined$availability_365
df_combined$score[is.infinite(df_combined$score)] <- NA
df_combined <- na.omit(df_combined)
df_combined <- subset(df_combined, select = c(2:5, 6:7, 10:13))

model <- train(formula = score~., data = df_combined, method = "lm")
model <- lm(formula = score~., data = df_combined)
summary(model)
