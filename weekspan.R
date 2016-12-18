#Get weekspan of new account creations
library(plyr)
library(stringr)
library(date)
library(ggplot2)
library(rpart)
library(caret)
library(tree)

d <- read.csv("train_users_2.csv", header = T)
s <- read.csv("train_users_2.csv", header = T)
t <- read.csv("test_users.csv", header = T)
df <- s
df <- subset(df, select = c(5:7, 9:16))

creation <- count(d$date_account_created)
lines(creation, col = "red")

booking <- count(d$date_first_booking)
lines(booking, col = "blue")

# Compile into days of the week
d$date_account_created <- as.Date(d$date_account_created)
days <- table(format(d$date_account_created, '%w'))
barplot(days, names.arg = c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"), 
        xlab = "Days of the Week", ylab = "Number of accounts created")

rays <- table(format(d$date_account_created, '%m'))

d$date_first_booking[d$date_first_booking == ""] <- NA
d <- na.omit(d)
d$date_first_booking <- as.Date(d$date_first_booking)
hays <- table(format(d$date_first_booking, '%w'))
hays <- data.frame(hays)
bays <- rbind(days, hays)

barplot(hays, names.arg = c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"),
        xlab = "Days of the Week", ylab = "Number of accounts", col = c("red", "blue"),
        beside = F)
#ggplot
day <- c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT")

p <- ggplot(hays, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + scale_x_discrete(labels = day) + guides(fill = F) + xlab("Days of the Week") + ylab("Count of First Bookings")
p

lays <- table(format(d$date_first_booking, '%m'))
pays <- rbind(rays, lays)
barplot(pays, xlab = "Months of the Year", ylab = "Frequency", col = c("red", "blue"),
        beside = F)

#predictive
dummy_features <- c("gender", "signup_method", "language", "affiliate_channel", "affiliate_provider",
                    "first_affiliate_tracked", "signup_app", "first_device_type", "first_browser")
dummies <- dummyVars(~ gender + signup_method + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df)
df_all <- as.data.frame(predict(dummies, newdata = df))
df_combined <- cbind(df$age, df_all, as.factor(df$country_destination))

model <- train(formula = score~., data = df_combined, method = "lm")
model <- tree(formula = df$country_destination~., data = df_combined)
summary(model)
