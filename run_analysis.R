library(tidyr)
library(dplyr)

projectRepo <- "c://users/preiss/rdocs/Getting-and-Cleaning-Data-Course-Project"
setwd(projectRepo)

#get the UCI HAR Dataset
furl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(furl, destfile = "Dataset.zip")
unzip("Dataset.zip")

setwd("./UCI HAR Dataset")

#read the measures files
x_test <- read.table(file = "./test/X_test.txt", sep = "")
x_train <- read.table(file = "./train/x_train.txt", sep = "")
#read the subjects file
subject_test <- readLines("./test/subject_test.txt")
subject_train <- readLines("./train/subject_train.txt")
#read the activity type file
y_test <- readLines("./test/y_test.txt")
y_train <- readLines("./train/y_train.txt")
#combine subject, activity, and measures
subject_test_table <- cbind(subject_test, y_test, x_test)
subject_train_table <- cbind(subject_train, y_train, x_train)
#create the column names
activity_labels <- read.table("activity_labels.txt")
activity_labels[,2] <- as.character(activity_labels[,2])
splitNames <- strsplit(readLines("features.txt"),split = " ")
secondElement <- function(x){x[2]}
featurColNames <- sapply(splitNames, secondElement)
featurColNames <- gsub("\\()","",featurColNames)
featurColNames <- gsub("-|\\,|\\(|\\)", "_", featurColNames)
featurColNames <- sub("_$", "", featurColNames)
allColNames <- c("subject", "activity", featurColNames)
colnames(subject_test_table) <- allColNames
colnames(subject_train_table) <- allColNames
x_test_mean_std <- subset(subject_test_table, select = grep("subject|activity|mean|std", names(subject_test_table)))
x_train_mean_std <- subset(subject_train_table, select = grep("subject|activity|mean|std", names(subject_train_table)))
merge_df <- merge(x_test_mean_std, x_train_mean_std, all = TRUE)
merge_df$activity <- factor(merge_df$activity, levels = activity_labels[,1], labels = activity_labels[,2])
head(merge_df[1:5])
#part 2 of assignment
#compute the mean on columns 2:X by subject and activity
aggregate_df <- aggregate(merge_df[, 3:length(merge_df)], by = list(merge_df$subject, merge_df$activity), FUN = mean)
colnames(aggregate_df)[1] <- "subject"
colnames(aggregate_df)[2] <- "activity"
head(aggregate_df[1:5])
setwd(projectRepo)
write.table(aggregate_df, "tidy.txt")
