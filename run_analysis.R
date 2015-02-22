
require(dplyr)

#############################################################################
## Directory and file locations

data_dir = "./data"
features_txt = paste(data_dir, "/features.txt", sep = "")
activity_labels_txt = paste(data_dir, "/activity_labels.txt", sep = "")
x_train_txt = paste(data_dir, "/train/X_train.txt", sep = "")
y_train_txt = paste(data_dir, "/train/y_train.txt", sep = "")
subject_train_txt = paste(data_dir, "/train/subject_train.txt", sep = "")
x_test_txt = paste(data_dir, "/test/X_test.txt", sep = "")
y_test_txt = paste(data_dir, "/test/y_test.txt", sep = "")
subject_test_txt = paste(data_dir, "/test/subject_test.txt", sep = "")


#############################################################################
## Load raw data

features <- read.table(features_txt, colClasses = c("character"))
activity_labels <- read.table(activity_labels_txt, 
                              col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_txt)
y_train <- read.table(y_train_txt)
subject_train <- read.table(subject_train_txt)
x_test <- read.table(x_test_txt)
y_test <- read.table(y_test_txt)
subject_test <- read.table(subject_test_txt)


#############################################################################
## For each record it is provided:
## ======================================
##   
## - A 561-feature vector with time and frequency domain variables (x_test 
##   and x_train).
## - Its activity label (y_test and y_train).
## - An identifier of the subject who carried out the experiment (subject_test 
##    and subject_train).
#############################################################################


#############################################################################
## Appropriately label the data set with descriptive variable names. 

names(x_train) <- names(x_test) <- features[,2]
names(y_train) <- names(y_test) <- c("ActivityId")
names(subject_train) <- names(subject_test) <- c("SubjectId")


#############################################################################
## Use descriptive activity names to name the activities in the data set

y_train <- join(y_train, activity_labels, by = "ActivityId")
y_test <- join(y_test, activity_labels, by = "ActivityId")


#############################################################################
## Merge the training and the test sets to create one data set.

training_data <- cbind(x_train, cbind(subject_train, y_train))
test_data <- cbind(x_test, cbind(subject_test, y_test))
combined_data <- rbind(training_data, test_data)


#############################################################################
## Extract only the measurements on the mean and standard deviation for 
## each measurement. 

## find vatiables containing 'mean' and 'sdt'
mean_std_measures <- c(grep("mean", features[,2]), grep("std", features[,2]))

## keep variables: 562 is 'SubjectId', 564 is 'Activity'
combined_data <- combined_data[, c(mean_std_measures, 562, 564) 


#############################################################################
## From the data set in the previous step, create a second, independent tidy data 
## set with the average of each variable for each activity and each subject.

tidy <- aggregate(combined_data[1:79], FUN=mean,
                  by = list(Subject = combined_data$SubjectId, 
                            Activity = combined_data$Activity))



#############################################################################
## writing the new tidy dataset to a text file

write.table(tidy, "./tidy.txt", sep=",", row.names=FALSE)

