## Peer Graded Assignment: Getting and Cleaning Data Course Project

# 0. Prepare: Download, unzip, list, and read, raw data files.
## Download file
if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile="./data/Dataset.zip")

## Unzip file
unzip(zipfile="./data/Dataset.zip", exdir="./data")

## List files
path_rf <- file.path("./data", "UCI HAR Dataset")
files <- list.files(path_rf, recursive = TRUE)
files

## Read Activity files
dataActivityTest <- read.table(file.path(path_rf, "test", "Y_test.txt"), header = FALSE)
dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"), header = FALSE)
## Read Subject files
dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"), header = FALSE)
dataSubjectTest <- read.table(file.path(path_rf, "test", "subject_test.txt"), header = FALSE)
## Read Features files
dataFeaturesTest <- read.table(file.path(path_rf, "test", "X_test.txt"), header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"), header = FALSE)

# 1. Merges the training and the test sets to create one data set.
##Concatenate the data tables by rows
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataActivity <- rbind(dataActivityTrain, dataActivityTest)
dataFeatures <- rbind(dataFeaturesTrain, dataFeaturesTest)
## Set variable names
names(dataSubject) <- c("subject")
names(dataActivity) <- c("activity")
dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"), head = FALSE)
names(dataFeatures) <- dataFeaturesNames$V2 ## V2 is a variable of table dataFeaturesNames
## Merge columns to get the data frame, Data, for all data
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)
### Sample output
#### > str(Data)
#### 'data.frame':    10299 obs. of  563 variables:
#### #### $ tBodyAcc-mean()-X                   : num  0.289 0.278 0.28 0.279 0.277 ...
#### $ tBodyAcc-mean()-Y                   : num  -0.0203 -0.0164 -0.0195 -0.0262 -0.0166 ...
#### $ tBodyAcc-mean()-Z                   : num  -0.133 -0.124 -0.113 -0.123 -0.115 ...
#### $ tBodyAcc-std()-X                    : num  -0.995 -0.998 -0.995 -0.996 -0.998 ...
#### $ tBodyAcc-std()-Y                    : num  -0.983 -0.975 -0.967 -0.983 -0.981 ...

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## Subset Name of Features by mean and standard deviation
subdataFeaturesNames <- dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
## Subset data fram, Data, by selected names in Features
selectedNames <- c(as.character(subdataFeaturesNames), "subject", "activity")
Data <- subset(Data, select = selectedNames)
### Sample output
#### > str(Data)
#### 'data.frame':    10299 obs. of  68 variables:
#### $ tBodyAcc-mean()-X          : num  0.289 0.278 0.28 0.279 0.277 ...
#### $ tBodyAcc-mean()-Y          : num  -0.0203 -0.0164 -0.0195 -0.0262 -0.0166 ...
#### $ tBodyAcc-mean()-Z          : num  -0.133 -0.124 -0.113 -0.123 -0.115 ...
#### $ tBodyAcc-std()-X           : num  -0.995 -0.998 -0.995 -0.996 -0.998 ...
#### $ tBodyAcc-std()-Y           : num  -0.983 -0.975 -0.967 -0.983 -0.981 ...

# 3. Uses descriptive activity names to name the activities in the data set.
## Read descriptive activity names from "activity_labels.txt"
activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"), header = FALSE)
## Factorize variable, activity, in the data frame, Data, using descriptive activity names
Data$activity <- factor(Data$activity);
Data$activity <- factor(Data$activity, labels = as.character(activityLabels$V2))
### Sample output
#### > str(Data$activity)
#### Factor w/ 6 levels "WALKING","WALKING_UPSTAIRS",..: 5 5 5 5 5 5 5 5 5 5 ...

# 4. Appropriately labels the data set with descriptive variable names
## Label Names of Features with descriptive variable names
### t -> time
names(Data) <- gsub("^t", "time", names(Data))
### f -> frequency
names(Data) <- gsub("^f", "frequency", names(Data))
### Acc -> Accelerometer
names(Data) <- gsub("Acc", "Accelerometer", names(Data))
### Gyro -> Gyroscope
names(Data) <- gsub("Gyro", "Gyroscope", names(Data))
### Mag -> Magnitude
names(Data) <- gsub("Mag", "Magnitude", names(Data))
### BodyBody -> Body
names(Data) <- gsub("BodyBody", "Body", names(Data))
#### Sample output
##### > names(Data)
##### [1] "timeBodyAccelerometer-mean()-X"                 "timeBodyAccelerometer-mean()-Y"                
##### [3] "timeBodyAccelerometer-mean()-Z"                 "timeBodyAccelerometer-std()-X"                 
##### [5] "timeBodyAccelerometer-std()-Y"                  "timeBodyAccelerometer-std()-Z"                 
##### [7] "timeGravityAccelerometer-mean()-X"              "timeGravityAccelerometer-mean()-Y"             
##### [9] "timeGravityAccelerometer-mean()-Z"              "timeGravityAccelerometer-std()-X"              

# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
## 
library(plyr);
Data2 <- aggregate(. ~subject + activity, Data, mean)
Data2 <- Data2[order(Data2$subject, Data2$activity),]
write.table(Data2, file = "tidydata.txt", row.name = FALSE)
### For tidydata.txt, go to https://github.com/DataBender88/Getting-and-Cleaning-Data-Course-Project

# Thank you for grading my assignment.  :)