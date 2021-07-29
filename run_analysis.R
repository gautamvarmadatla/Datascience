library(dplyr)
library(tidyr)

# Download the data from website and unzip the downloaded dataset

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "dataset.zip", mode = "wb")
unzip("dataset.zip")

#__________________________________________________________________________________________________

# Reading Files

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

feature_name <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)

activity_label <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)

#_________________________________________________________________________________________________

#1. Merging test and train Data set

names <- c("subject_id", feature_name$V2, "activity_id")
merge_train <- cbind(subject_train, X_train, y_train)
merge_test <- cbind(subject_test, X_test, y_test)
mergedata <- rbind(merge_train, merge_test)
colnames(mergedata) <- names

colnames(activity_label) <- c("activity_id", "activity_name")

#_________________________________________________________________________________________________

#2. Extract the measurements based on `mean` and `std` for each sample

feature_mean_std <- mergedata[, grepl("subject_id|activity_id|mean|std", names)]

#_________________________________________________________________________________________________

#3. 

feature_named_act <- merge(feature_mean_std, activity_label, by = "activity_id")
feature_named_act$"activity_id" = NULL


#_________________________________________________________________________________________________

#4 Appropriately labels the data set with descriptive variable names. 

names(feature_named_act)[2] = "activity"
names(feature_named_act)<-gsub("Acc", "Accelerometer", names(feature_named_act))
names(feature_named_act)<-gsub("Gyro", "Gyroscope", names(feature_named_act))
names(feature_named_act)<-gsub("BodyBody", "Body", names(feature_named_act))
names(feature_named_act)<-gsub("Mag", "Magnitude", names(feature_named_act))
names(feature_named_act)<-gsub("^t", "Time", names(feature_named_act))
names(feature_named_act)<-gsub("^f", "Frequency", names(feature_named_act))
names(feature_named_act)<-gsub("tBody", "TimeBody", names(feature_named_act))
names(feature_named_act)<-gsub("-mean()", "Mean", names(feature_named_act), ignore.case = TRUE)
names(feature_named_act)<-gsub("-std()", "STD", names(feature_named_act), ignore.case = TRUE)
names(feature_named_act)<-gsub("-freq()", "Frequency", names(feature_named_act), ignore.case = TRUE)
names(feature_named_act)<-gsub("angle", "Angle", names(feature_named_act))
names(feature_named_act)<-gsub("gravity", "Gravity", names(feature_named_act))

Final <- aggregate(. ~ subject_id + activity_name, feature_named_act, FUN = mean)
Final <- arrange(Final, subject_id, activity_name)

#___________________________________________________________________________________________________

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
#variable for each activity and each subject.


write.table(Final, "Final.txt", row.names = FALSE)
str(Final)

