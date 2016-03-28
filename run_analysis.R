##
# This run_analysis.R script does the following:
#
# .Merges the training and the test sets to create one data set;
# .Extracts only the measurements on the mean and standard deviation for each
#  measurement.
# .Uses descriptive activity names to name the activities in the data set
# .Appropriately labels the data set with descriptive variable names.
# .From the data set in step 4, creates a second, independent tidy data set
#  with the average of each variable for each activity and each subject.
##

## Loading needed libraries
library(plyr)

##
# 1. Downloading and unziping the data for the project.
##

# Verifying if the data directory exists, for unzip the datasets.
# If not, create it
if (!file.exists("./data")) {
        dir.create("./data")
}

# verifying if the file has already been downloaded.
# If not, we download in the the data directory.
if (!file.exists("./data/getdata_UCI HAR Dataset.zip")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, "./data/getdata_UCI HAR Dataset.zip", mode = "wb")
}

# Extracting sets in the data directory
unzip("./data/getdata_UCI HAR Dataset.zip", exdir = "./data") 

##
# 2. Reading data sets.
##

# Reading features and activity labels data sets
features = read.table("./data/UCI HAR Dataset/features.txt"); 
activity_labels = read.table('./data/UCI HAR Dataset/activity_labels.txt');
colnames(activity_labels) = c('activityId','activityType');

# Reading test data sets
test_subject = read.table('./data/UCI HAR Dataset/test/subject_test.txt');
colnames(test_subject) = "subjectId";

test_x = read.table('./data/UCI HAR Dataset/test/x_test.txt');
colnames(test_x) = features[,2];

test_y = read.table('./data/UCI HAR Dataset/test/y_test.txt');
colnames(test_y) = "activityId";

# Reading train data sets
train_subject = read.table('./data/UCI HAR Dataset/train/subject_train.txt');
colnames(train_subject) = "subjectId";

train_x = read.table('./data/UCI HAR Dataset/train/x_train.txt');
colnames(train_x) = features[,2];

train_y = read.table('./data/UCI HAR Dataset/train/y_train.txt');
colnames(train_y) = "activityId";

##
# 3. Merging the training and the test data sets.
##

# Preparing test and train data sets
test_data <- cbind(test_subject,test_y,test_x)
train_data <- cbind(train_subject,train_y,train_x)

# Merging test and train data sets
merged_data <- rbind(test_data,train_data)

##
# 4. Appropriately labeling the data set with descriptive variable names
##

descriptive_names <- as.character(features[,2])
descriptive_names <- c("subjectId", "activityId", descriptive_names)
descriptive_names <- gsub('-mean', 'Mean', descriptive_names)
descriptive_names <- gsub('-std', 'Std', descriptive_names)
descriptive_names <- gsub('[-()]', '', descriptive_names)
colnames(merged_data)  <- descriptive_names

##
# 5. Extracting only the measurements on the mean and standard deviation for
#    each measurement.
##
boolean_selection = (grepl("mean", descriptive_names, ignore.case = TRUE)
           | grepl("std", descriptive_names, ignore.case = TRUE)
           | grepl("activityId", descriptive_names)
           | grepl("subjectId", descriptive_names))

final_data = merged_data[boolean_selection]

##
# 6. Using descriptive activity names to name the activities in the data set
#
final_merged_data = merge(final_data, activity_labels, by = "activityId", 
                          all.x = TRUE)

##
# 7. Creating a tidy data set with the average of each variable for each
# activity and each subject.
##
tidy_data <- ddply(final_merged_data, .(activityId, subjectId),
                   function(x) colMeans(x[, 3:88]))

# including descriptive activity names by merging the tidyDS with activityLabels
tidy_data = merge(tidy_data, activity_labels, by = "activityId", all.x = TRUE)


# Writing tidy_data to a file in the current directory
write.table(tidy_data, "./tidyData.txt", row.names = FALSE)
