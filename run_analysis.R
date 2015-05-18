getwd()
library(data.table)
library(stringr)
library(dplyr)
library(memisc)

# download zip
# NOTE: due to my firewall I had to download and unzip manually, so this block hasn't been tested
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile="./Dataset.zip",method="curl")
unzip(zipfile="./Dataset.zip",exdir=".")
# END untested block

# load to R data sets

xtrain<-read.table("X_train.txt")
ytrain<-read.table("Y_train.txt")
strain<-read.table("subject_train.txt")

xtest<-read.table("X_test.txt")
ytest<-read.table("Y_test.txt")
stest<-read.table("subject_test.txt")

activity<-read.table("activity_labels.txt")
features<-read.table("features.txt",head=FALSE)

### 1. Merges the training and the test sets to create one data set.

# add test/train group column to subject datasets
strain$group<-"train"
stest$group<-"test"

# merge: stack training and test data sets
alls<-rbind(strain,stest)
ally<-rbind(ytrain,ytest)
allx<-rbind(xtrain,xtest)

# name columns, TIDY: translate features to all lower case
names(alls)<-c("subject","group")
setnames(ally,"activitycode")
names(allx)<-tolower(features[,2])

# merge subjects, activity codes and measurements
combo<-cbind(alls,ally,allx)
colnames(combo)

### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#identify columns to keep

cols<-grep("subject|group|activitycode|mean|std",colnames(combo))
cols

#extract columns

combokeep<-combo[,cols]
dim(combokeep)

# [1] 10299    89 - verified extract

### 3. Uses descriptive activity names to name the activities in the data set

# rename activity data set columns
names(activity)<-c("activitycode","activityname")
activity

#add activity name to combokeep, default merges on common name: "activitycode"

combowithactivity<-merge(activity,combokeep)
dim(combowithactivity)
# [1] 10299    90 - verified merge

# TIDY: drop activitycode (redundant with activity name)
# note: in practice i'd probably keep it, easier to remerge later if needed

combowithactivity$activitycode<-NULL
dim(combowithactivity)
# [1] 10299    89 - verified column drop

### 4. Appropriately labels the data set with descriptive variable names. 

## TIDY: clarify column names based on README details:

# "(prefix 't' to denote time)..."
names(combowithactivity)<-gsub("^t","time",names(combowithactivity))

# "Also the magnitude of these three-dimensional signals were..."
names(combowithactivity)<-gsub("mag","magnitude",names(combowithactivity))

# "(Note the 'f' to indicate frequency domain signals)." 
names(combowithactivity)<-gsub("^f","freq",names(combowithactivity))

# "train/Inertial Signals/total_acc_x_train.txt': The acceleration signal..."
# 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal..." 
names(combowithactivity)<-gsub("acc","acceleration",names(combowithactivity))

# "train/Inertial Signals/body_gyro_x_train.txt': 
# The angular velocity vector measured by the gyroscope..."
names(combowithactivity)<-gsub("gyro","gyroscope",names(combowithactivity))

# drop -() characters
names(combowithactivity)<-gsub("-","",names(combowithactivity))
names(combowithactivity)<-gsub("\\(","",names(combowithactivity))
names(combowithactivity)<-gsub("\\)","",names(combowithactivity))
names(combowithactivity)<-gsub("\\,","",names(combowithactivity))

## TIDY: sort on group, subject, and activity

combowithactivity<-arrange(combowithactivity,group,subject,activityname)

### 5. From the data set in step 4, creates a second, independent tidy data set 
### with the average of each variable for each activity and each subject.

DT <- data.table(combowithactivity)
DT$group<-NULL
mydata<-aggregate(.~subject+activityname,DT,mean)
mydata<-arrange(mydata,subject,activityname)

dim(mydata)
# 180  88

# write to text file for submission
write.table(mydata,"mydata.txt",row.name=FALSE)

#create codebook
cboutput<-codebook(data.set(mydata))
capture.output(cboutput,file="cb.txt")

### End Here. #############################################################

# additional code for examining the individual data sets 
# used these before combining them

dim(xtrain)
# [1] 7352  561
dim(ytrain)
# [1] 7352    
dim(strain)
# [1] 7352    

dim(xtest)
# [1] 2947  561
dim(ytest)
# [1] 2947    
dim(stest)
# [1] 2947    

table(ytrain)
# ytrain
# 1    2    3    4    5    6 
# 1226 1073  986 1286 1374 1407

table(strain)
# strain
# 1   3   5   6   7   8  11  14  15  16  17  19  21  22  23  25  26  27  28  29  30 
# 347 341 302 325 308 281 316 323 328 366 368 360 408 321 372 409 392 376 382 344 383 

table(ytest)
# ytest
# 1   2   3   4   5   6 
# 496 471 420 491 532 537 

table(stest)
# stest
# 2   4   9  10  12  13  18  20  24 
# 302 317 288 294 320 327 364 354 381 

activity
# > activity
# V1                 V2
# 1  1            WALKING
# 2  2   WALKING_UPSTAIRS
# 3  3 WALKING_DOWNSTAIRS
# 4  4            SITTING
# 5  5           STANDING
# 6  6             LAYING

features
dim(features)
# [1] 561   2




