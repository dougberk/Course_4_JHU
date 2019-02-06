library(dplyr)
library(data.table)
library(readtext)

setwd("//chnas06/MKT-Data/Retention/RET Analytics/User/Doug B/Continued Learning/John Hopkins Data Science Courses/Getting and Cleaning Data/Week 4/UCI HAR Dataset/")
data.train.x = read.table("train/X_train.txt")
data.train.y = read.table("train/y_train.txt")
subject.train = read.table("train/subject_train.txt")

data.test.x = read.table("test/X_test.txt")
data.test.y = read.table("test/y_test.txt")
subject.test = read.table("test/subject_test.txt")

activity.labels = read.table("activity_labels.txt")
features = read.table("features.txt") 

#We need to assign the names of the columns for our datasets
#First we will do the train data
names(data.train.x) = features$V2
names(subject.train) = 'subject'
names(data.train.y) = 'activity'

#Next we will do the test data
names(data.test.x) = features$V2
names(subject.test) = 'subject'
names(data.test.y) = 'activity'

#We will combine the three different datasets into 1 for each train / test category
data.train = cbind(subject.train,data.train.y,data.train.x)
data.train$test.train = 'train'
data.test = cbind(subject.test,data.test.y,data.test.x)
data.test$test.train = 'test'

#We will now combine the two main datasets into 1
data.combine = rbind(data.train,data.test)

#we will now extract only the measurements that are exactly mean() or std(), the \\b means an exact match
index=grep('\\bmean()\\b|\\bstd()\\b', features[,2])
data.mean.std = data.combine[,c(1,2,564,index)]

#Now we need to bring in the activity name based on the activity type column
names(activity.labels) = c('activity','activity.label')
data.activity = data.mean.std %>% 
     left_join(activity.labels)


#Now we need to change the variable names to descriptive names
names.descriptive = names(data.activity)
names.descriptive = gsub("^t","TimeDomain_",names.descriptive)
names.descriptive = gsub("^f","FrequencyDomain_",names.descriptive)
names.descriptive <- gsub("[(][)]", "", names.descriptive)
names.descriptive <- gsub("Acc", "Accelerometer", names.descriptive)
names.descriptive <- gsub("Gyro", "Gyroscope", names.descriptive)
names.descriptive <- gsub("Mag", "Magnitude", names.descriptive)
names.descriptive <- gsub("-mean-", "_Mean_", names.descriptive)
names.descriptive <- gsub("-std-", "_StandardDeviation_", names.descriptive)
names.descriptive <- gsub("-", "_", names.descriptive)
names(data.activity) <- names.descriptive

#last thing we need to do is create a new tidy dataset
data.tidy = aggregate(data.activity[,6:69], by = list(activity = data.activity$activity, subject = data.activity$subject),FUN = mean)


