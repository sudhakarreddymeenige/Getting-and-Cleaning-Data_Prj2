## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## This program is created step by step process to easy of debugging to ensure the desired results

## Read in subject training/test data:
dtTrain <- fread("train/subject_train.txt")
dtTest  <- fread("test/subject_test.txt" )

## Read in activity training/test data:

## Helper function to read in large data sets correctly:
readtable <- function (file) {
  dataf <- read.table(file)
  dt1 <- data.table(dataf)
  
  return(dt1)
}

dtATrain <- readtable("train/Y_train.txt")
dtATest <- readtable("test/Y_test.txt")
dtTrain <- readtable("train/X_train.txt")
dtTest <- readtable("test/X_test.txt")

## Merge training and test datasets::
dtSubject <- rbind(dtTrain, dtTest)
dtActivity <- rbind(dtATrain, dtATest)

## Set column names:
setnames(dtSubject, "V1", "subject")
setnames(dtActivity, "V1", "activityNum")

dt1 <- rbind(dtTrain, dtTest)

dtSubject <- cbind(dtSubject, dtActivity)
dt1 <- cbind(dtSubject, dt1)

setkey(dt1, subject, activityNum)

## Below statemet to write the data into table dt

write.table(dt,"fulldata.txt",row.name=FALSE)

##################################################################################
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

dtFeatures <- fread("features.txt")
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode
select <- c(key(dt1), dtFeatures$featureCode)
dt2 <- dt1[, select, with=FALSE]

##################################################################################
## 3. Uses descriptive activity names to name the activities in the data set

dtActivityNames <- fread("activity_labels.txt")
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

dt3 <- merge(dt2, dtActivityNames, by="activityNum", all.x=TRUE)

##################################################################################
## 4. Appropriately labels the data set with descriptive variable names. 

setkey(dt3, subject, activityNum, activityName)

##################################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dt3 <- data.table(melt(dt3, key(dt3), variable.name="featureCode"))

dt3 <- merge(dt3, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

dt3$activity <- factor(dt3$activityName)
dt3$feature <- factor(dt3$featureName)

grep_feature <- function (regex) {
  grepl(regex, dt3$feature)
}

## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grep_feature("^t"), grep_feature("^f")), ncol=nrow(y))
dt3$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grep_feature("Acc"), grep_feature("Gyro")), ncol=nrow(y))
dt3$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grep_feature("BodyAcc"), grep_feature("GravityAcc")), ncol=nrow(y))
dt3$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x<- matrix(c(grep_feature("mean()"), grep_feature("std()")), ncol=nrow(y))
dt3$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

## Features with 1 category

dt3$featJerk <- factor(grep_feature("Jerk"), labels=c(NA, "Jerk"))
dt3$featMagnitude <- factor(grep_feature("Mag"), labels=c(NA, "Magnitude"))

## Features with 3 categories

n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grep_feature("-X"), grep_feature("-Y"), grep_feature("-Z")), ncol=nrow(y))
dt3$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

r1 <- nrow(dt3[, .N, by=c("feature")])
r2 <- nrow(dt3[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

setkey(dt3, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt3[, list(count = .N, average = mean(value)), by=key(dt3)]

##################################################################################
# Please upload the tidy data set created in step 5 of the instructions. Please upload your data set as a txt file created with write.table() using row.name=FALSE (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).

## this statement will write final data into tisy.data.txt file.

write.table(dtTidy, "tidy.data.txt", row.name=FALSE)