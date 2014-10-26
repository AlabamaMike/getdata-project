#Load plyr lib for data manipulation
library(plyr)

#Get archive of datafiles, unzip it to a data directory
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip ", "dataset.zip", "curl")
unzip("dataset.zip", exdir = "data")

## Load tables for all required data
x_testDF <- read.table("./data/UCI HAR Dataset/test/X_test.txt", sep="\n", strip.white=T)
x_trainDF <- read.table("./data/UCI HAR Dataset/train/X_train.txt", sep="\n", strip.white=T)
y_testDF <- read.table("./data/UCI HAR Dataset/test/y_test.txt", sep="\n", strip.white=T)
y_trainDF <- read.table("./data/UCI HAR Dataset/train/y_train.txt", sep="\n", strip.white=T)
featuresDF <- read.table("./data/UCI HAR Dataset/features.txt", sep="\n", strip.white=T)
subj_trainDF <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", sep="\n", strip.white=T)
subj_testDF <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", sep="\n", strip.white=T)

# Create single observations for each column
x_trainDF <- ldply(strsplit(gsub(" {2,}", " ", x_trainDF$V1 ), " "))
x_testDF <- ldply(strsplit(gsub(" {2,}", " ", x_testDF$V1 ), " "))

# Create wide datasets that combines training, test, and subject data
trainDF <- cbind(y_trainDF, subj_trainDF, x_trainDF)
testDF <- cbind(y_testDF, subj_testDF, x_testDF)

#1: Merge test and training datasets into one data frame
mergeDF <- rbind(testDF, trainDF)

# Replace activity codes with labels
featuresDF <- gsub("^[0-9]+ ", "", featuresDF$V1)

#2: Extract only the measurements on the mean and standard deviation for each measurement
study_features <- grepl("mean|std", featuresDF)
mergeDF <- mergeDF[, c(TRUE, TRUE, study_features)]

#3: Use descriptive activity names to name the activities in the merged data
column_names <- c("activity", "subject", featuresDF[study_features])

#4: Appropriately labels the data set with descriptive variable names. 
colnames(mergeDF) <- column_names

# Cast feature values as numeric values
for (i in 3:ncol(mergeDF)){
    mergeDF[,i] <- as.numeric(mergeDF[,i])
}
# Write out tidy, wide dataset
write.table(mergeDF, file="tidy_dataset.txt")

# 5.- Create a second, independent tidy data set with the average of each variable for each activity and subject
means <- aggregate(mergeDF[,3] ~ mergeDF$subject + mergeDF$activity, data = mergeDF, FUN = mean)

for (i in 4:ncol(mergeDF)){
    means[,i] <- aggregate( mergeDF[,i] ~ mergeDF$subject + mergeDF$activity, data = mergeDF, FUN = mean )[,3]
}
colnames(means) <- column_names
# Write out means dataset
write.table(means, file="means_dataset.txt")
