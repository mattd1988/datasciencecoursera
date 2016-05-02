if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./data/wearable.zip")
unzip(zipfile = "./data/wearable.zip")
path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
XTest<- read.table("UCI HAR Dataset/test/X_test.txt")
YTest<- read.table("UCI HAR Dataset/test/Y_test.txt")
SubjectTest <-read.table("UCI HAR Dataset/test/subject_test.txt")
XTrain<- read.table("UCI HAR Dataset/train/X_train.txt")
YTrain<- read.table("UCI HAR Dataset/train/Y_train.txt")
SubjectTrain <-read.table("UCI HAR Dataset/train/subject_train.txt")
features<-read.table("UCI HAR Dataset/features.txt")
activity<-read.table("UCI HAR Dataset/activity_labels.txt")
dataSubject <- rbind(SubjectTrain, SubjectTest)
dataActivity<- rbind(XTrain, XTest)
dataFeatures<- rbind(YTrain, YTest)
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
names(dataFeatures)<- dataFeatures$V2
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)
index<-grep("mean\\(\\)|std\\(\\)", features[,2])
dataActivity<-dataActivity[,index]
dataFeatures[,1]<-activity[dataFeatures[,1],2]
names<-features[index,2]
names(dataActivity)<-names
names(dataSubject)<-"SubjectID"
names(dataFeatures)<-"Activity"
CleanedData<-cbind(dataSubject, dataFeatures, dataActivity)
CleanedDataTable<-data.table(CleanedData)
TidyData <- CleanedDataTable[, lapply(.SD, mean), by = 'SubjectID,Activity']
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)