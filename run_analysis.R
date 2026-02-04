setwd("E:/Parvartanam")
library(dplyr)

#Fetching the train dataset

train_x<-read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/train/X_train.txt")
train_y<-read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/train/y_train.txt")
train_subject<-read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/train/subject_train.txt")

#Setting Variable names for train dataset 
names(train_y)<-c("Activity")
names(train_subject)<-c("Activity_performedby_Volunteer")

#Appending the train dataset into single data table
train<-mutate(train_x,"Activity"=train_y$Activity,"Volunteer"=train_subject$Activity_performedby_Volunteer)

#Fetching the test dataset
test_x<-read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/test/X_test.txt")
test_y<-read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/test/y_test.txt")
test_subject<-read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/test/subject_test.txt")

#Setting Variable names for test dataset 
names(test_subject)<-c("Activity_performedby_Volunteer")
names(test_y)<-c("Activity")

#Appending the train dataset into single data table
test<-mutate(test_x,"Activity"=test_y$Activity,"Volunteer"=test_subject$Activity_performedby_Volunteer)

#Combining the train and test data as required in point 1 of the Project
combined_dt<-rbind(train,test)

#Setting Variable names for combined dataset to make it Tidy
features<-read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/features.txt")
names(features)<-c("feature_num","feature")
names(combined_dt)<-c(features$feature,"Activity","Volunteer")

#finding the columns containing mean or std text and select those columns only as required in point 2 of the Project
grep("mean|std",colnames(combined_dt))
dt_mean_std<-combined_dt[,grep("mean|std",colnames(combined_dt))]

#fetching data for the activity labels and naming the variables 
activity<-read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/activity_labels.txt")
names(activity)<-c("Activity_Class","Activity")

#checking the data type for the Activity variable in combined data and processing it to show description of activities in combined data as required in point 3 of the Project
str(combined_dt$Activity)
combined_dt$Activity<-as.character(combined_dt$Activity)
str(combined_dt$Activity)
str(activity)
combined_dt$Activity<-gsub(activity$Activity_Class[1],activity$Activity[1],combined_dt$Activity)
combined_dt$Activity<-gsub(activity$Activity_Class[2],activity$Activity[2],combined_dt$Activity)
combined_dt$Activity<-gsub(activity$Activity_Class[3],activity$Activity[3],combined_dt$Activity)
combined_dt$Activity<-gsub(activity$Activity_Class[4],activity$Activity[4],combined_dt$Activity)
combined_dt$Activity<-gsub(activity$Activity_Class[5],activity$Activity[5],combined_dt$Activity)
combined_dt$Activity<-gsub(activity$Activity_Class[6],activity$Activity[6],combined_dt$Activity)
View(combined_dt)

#setting unique column names for the combined data and grouping the data based on activity and volunteer variables and finally calculating the mean as a seperate dataset as required in point 4 and 5 of the Project
colnames(combined_dt) <- make.names(colnames(combined_dt), unique = TRUE)
grouped_dt<- group_by(combined_dt,Activity,Volunteer)
avg_data <- aggregate(. ~ Activity + Volunteer,data = combined_dt,FUN = mean)
View(avg_data)
combined_dt