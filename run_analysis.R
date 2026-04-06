# Set working directory
setwd("E:/Parvartanam")

library(dplyr)

#-------------------------------
# 1. Load train data
#-------------------------------
train_x <- read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/train/X_train.txt")
train_y <- read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/train/y_train.txt",
                      col.names = "Activity")
train_subject <- read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/train/subject_train.txt",
                            col.names = "Volunteer")

train <- cbind(train_x, Activity = train_y$Activity, Volunteer = train_subject$Volunteer)

#-------------------------------
# 2. Load test data
#-------------------------------
test_x <- read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/test/X_test.txt")
test_y <- read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/test/y_test.txt",
                     col.names = "Activity")
test_subject <- read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/test/subject_test.txt",
                           col.names = "Volunteer")

test <- cbind(test_x, Activity = test_y$Activity, Volunteer = test_subject$Volunteer)

#-------------------------------
# 3. Combine datasets
#-------------------------------
combined_dt <- rbind(train, test)

#-------------------------------
# 4. Assign feature names
#-------------------------------
features <- read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/features.txt",
                       col.names = c("feature_num", "feature"))

colnames(combined_dt) <- c(features$feature, "Activity", "Volunteer")

# Clean column names
colnames(combined_dt) <- make.names(colnames(combined_dt), unique = TRUE)

#-------------------------------
# 5. Extract mean and std columns
#-------------------------------
dt_mean_std <- combined_dt %>%
  select(matches("mean|std"), Activity, Volunteer)

#-------------------------------
# 6. Replace activity numbers with names
#-------------------------------
activity_labels <- read.table("Coursera/getdata_projectfiles_UCI_HAR_Dataset/UCI_HAR_Dataset/activity_labels.txt",
                              col.names = c("Activity_ID", "Activity_Name"))

combined_dt$Activity <- factor(combined_dt$Activity,
                               levels = activity_labels$Activity_ID,
                               labels = activity_labels$Activity_Name)

#-------------------------------
# 7. Create tidy dataset (average)
#-------------------------------
avg_data <- combined_dt %>%
  group_by(Activity, Volunteer) %>%
  summarise(across(everything(), mean), .groups = "drop")

#-------------------------------
# 8. View result
#-------------------------------
View(avg_data)
