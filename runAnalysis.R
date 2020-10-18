data_path <- "/Users/roshan/Documents/UCI HAR Dataset /"

features_path <- paste(data_path, "features.txt", sep = "/")
features <- read.table(features_path)

activity_path <- paste(data_path, "activity_labels.txt", sep = "/")
activity_data <- read.table(activity_path)

X_train_path <- paste(data_path, "train", "X_train.txt", sep = "/")
X_train = read.table(X_train_path)
y_train_path <- paste(data_path, "train", "y_train.txt", sep = "/")
y_train <- read.table(y_train_path)

sub_train_path <- paste(data_path, "train", "subject_train.txt", sep = "/")
sub_train <- read.table(sub_train_path)

X_test_path <- paste(data_path, "test", "X_test.txt", sep = "/")
X_test <- read.table(X_test_path)

y_test_path <- paste(data_path, "test", "y_test.txt", sep = "/")
y_test <- read.table(y_test_path)

sub_test_path <- paste(data_path, "test", "subject_test.txt", sep = "/")
sub_test <- read.table(sub_test_path)

activity <- rbind(y_train, y_test)
subject <- rbind(sub_train, sub_test)

# 1. Merges the training and the test sets to create one data set.
dat <- rbind(X_train, X_test)
dat$subject <- subject$V1
dat$activity <- activity$V1

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
mean_std_pos <-c(grep("mean", features$V2), 
                           grep("std", features$V2))

dat_sel <- dat[, c(mean_std_pos)]
dat_sel$subject <- dat$subject
dat_sel$activity <- dat$activity

# 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- sapply(dat_sel$activity, FUN = function(x) {
  return(activity_data$V2[which(activity_data$V1==x)])
})

dat_sel$activity <- activity_labels

# 4. Appropriately labels the data set with descriptive variable names
features$V2 <- as.character(features$V2)
colnames(dat_sel) <- c(features$V2[mean_std_pos], "Subject", "Activity")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
avg_all <- aggregate(. ~ Subject + Activity, 
                     data = dat_sel, FUN = "mean")

write.table(avg_all, "FinalData.txt", row.name=FALSE)

str(avg_all)
