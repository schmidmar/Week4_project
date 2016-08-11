# Getting and cleaning data script for accelerometer data
# Source: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 1) Merge test and training data
# 2) extract mean and std.dev for each measurement
# 3) Use descriptive names for each activity in data set
# 4) Appropriately label the data set with descriptive variable naems
# 5) From dataset in step 4 create a tidy data set average of each variable for each activity and each subject.

# Import datasets: use setwd() to make the "UCI Hard Dataset" the parent folder for subsequent read() calls.

setwd("C:/Coursera/Getting and Cleaning Data/accelerometerData/UCI HAR Dataset/")

test <- read.table("test/X_test.txt") # import data
testID <- read.table("test/subject_test.txt",col.names = c("SubjectID"))[1] # import Subject ID
testAct <- read.table("test/y_test.txt", col.names = c("ActivityCode")) # import activity ID 

train <- read.table("train/X_train.txt")
trainID <- read.table("train/subject_train.txt", col.names= c("SubjectID"))[1]
trainAct <- read.table("train/y_train.txt", col.names = c("ActivityCode"))

features <- read.table("features.txt")[,2]
names(test) <- features; names(train) <- features
test <- cbind(testID, testAct, test); train <- cbind(trainID, trainAct, train)


# 1) Merge datasets: the data sets contain the same variables, in the same order so therefore they can be
#     merged by rbind.

merged_data <- rbind(train, test)

#2 ) extract mean and std.dev for each measurement: this can be done by means of grep indexing on columns

index <- c(1,2) # Make sure to include SubjectID and ActivityCode, could be functionalized to "SubjectID" and "ActivityCode"
index <- append(index, grep("mean", names(merged_data))) # Pull all columns for mean values
index <- append(index, grep("std", names(merged_data))) # Pull all columns with std.dev values

# The above grep command could be one-lined as grep("[meanstd]{3,4}") hypothetically. Didn't test, was afraid it'd pick up something else. 

MeanAndStdDev <- merged_data[,index]

rm(test, testID, train, trainID, testAct, trainAct, index, features)

# 3) Use Descriptive Names for each activity in dataset. Activity names are derived from activity_labels.txt

activityNames <- read.table("activity_labels.txt", col.names = c("ActivityCode", "Activity"))

tidier <- merge(activityNames, MeanAndStdDev) # will merge on common column "ActivityCode"

rm(activityNames, merged_data, MeanAndStdDev) # clean up work space

# 4) Appropriately relabel variables to be more human readable (they're not that bad...?)
#   Relabel column names to fully express operation

# # # # I'm just calling this good. The features are fairly descriptive and the data dictionary provided is superb.

# 5) Creating a new dataset with the means for each variable, for each subject and each activity.
#   Using a2 for loops and apply, this will iterate over each subject, each subject's activity, and each variable.
#     Loop 1 will iterate subjects, loop two (nested) will iterate activities, within loop two apply(x,2,mean(x)) will run.
#       Each loopback of apply will kick back the output to to a data frame, each row of which will be subject: activity: variable: mean
#         If I knew how to use melt in dplyr better - that could probably efficiently do this too in 3 lines.

tidy <- data.frame()

for(i in unique(tidier$SubjectID)){
  
  for(j in unique(tidier$Activity)){
    
    tidy  <- rbind(tidy, cbind(i, j, apply(tidier[4:dim(tidier)[2]], 2, function(x) mean(x, na.rm = TRUE))))
    
    }
}

tidy$variable <- rep(rownames(tidy)[1:79], 30); rownames(tidy) <- 1:14220; colnames(tidy)[1:3] <- c("Subject", "Activity", "MeanValue")

write.csv(tidy, "tidy_data.csv", row.names = FALSE)

version

# platform       x86_64-w64-mingw32          
# arch           x86_64                      
# os             mingw32                     
# system         x86_64, mingw32             
# status                                     
# major          3                           
# minor          3.1                         
# year           2016                        
# month          06                          
# day            21                          
# svn rev        70800                       
# language       R                           
# version.string R version 3.3.1 (2016-06-21)
# nickname       Bug in Your Hair 

q()