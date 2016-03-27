##Merges the training and the test sets to create one data set.
##Extracts only the measurements on the mean and standard deviation for each measurement.
##Uses descriptive activity names to name the activities in the data set
##Appropriately labels the data set with descriptive variable names.
##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {
        dir.create(path)
}
download.file(url, file.path(path, f))
executable <- file.path("C:", "Program Files (x86)", "7-Zip", "7z.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, f), "\""))
system(cmd)
pathIn <- file.path(path, "UCI HAR Dataset")
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
 setnames(dtSubject, "V1", "subject")
 dtActivity <- rbind(dtActivityTrain, dtActivityTest)
 setnames(dtActivity, "V1", "activityNum")
 dt <- rbind(dtTrain, dtTest)
 dtSubject <- cbind(dtSubject, dtActivity)
 dt <- cbind(dtSubject, dt)
 setkey(dt, subject, activityNum)
 dtFeatures <- fread(file.path(pathIn, "features.txt"))
 setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
 dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
 dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
 select <- c(key(dt), dtFeatures$featureCode)
 dt <- dt[, select, with = FALSE]
 dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
 setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
 dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)
 setkey(dt, subject, activityNum, activityName)
 dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
 dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", 
                           all.x = TRUE)
 dt$activity <- factor(dt$activityName)
 dt$feature <- factor(dt$featureName)
 grepthis <- function(regex) {
             grepl(regex, dt$feature)
         }
 n <- 2
 y <- matrix(seq(1, n), nrow = n)
 x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
 dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
 x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
 dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
 x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
 dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
 x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
 dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
 dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
 dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
 n <- 3
 y <- matrix(seq(1, n), nrow = n)
 x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
 dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))
 setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument,
                 featJerk, featMagnitude, featVariable, featAxis)
 dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]