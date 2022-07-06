##### Getting and Cleaning Data Course Project


# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


library(dplyr)

# download the data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Data/Data_Course_Project.zip",method="curl")

#unzip it
if (!file.exists("UCI HAR Dataset")){
  unzip("./Data/Data_Course_Project.zip")
}

# Load activity labels and features:
features <- read.table("./Data/UCI HAR Dataset/features.txt", col.names = c("n","features"))
activites <- read.table("./Data/UCI HAR Dataset/activity_labels.txt", col.names = c("class","activity"))

# Load test datasets:
subject_test <- read.table("./Data/UCI HAR Dataset/test/subject_test.txt", col.names ="subject")
X_test <- read.table("./Data/UCI HAR Dataset/test/X_test.txt",col.names = features$features)
y_test <- read.table("./Data/UCI HAR Dataset/test/y_test.txt",col.names = "labels")


# Load train datasets:
subject_train <- read.table("./Data/UCI HAR Dataset/train/subject_train.txt", col.names ="subject")
X_train <- read.table("./Data/UCI HAR Dataset/train/X_train.txt",col.names = features$features)
y_train <- read.table("./Data/UCI HAR Dataset/train/y_train.txt",col.names = "labels")

# 1. Merge the training and test sets to create one data set: 
test <- cbind(subject_test,y_test,X_test)
train <- cbind(subject_train,y_train,X_train)
combined <- rbind(train,test)

# 2. Extract only measurements on mean and standard deviation
meanStd_only <- grep(".*mean.*|.*std.*",names(combined),value=TRUE)
reducedSet <- combined[,c("subject","labels",meanStd_only)]
colnames(reducedSet)

# 3. Uses descriptive activity names to name the activities in the data set instead of class
reducedSet$labels <- activites[reducedSet$labels,2]

# 4.Appropriately labels the data set with descriptive variable names. 
colnames(reducedSet)
names(reducedSet)[2] = "activity"
names(reducedSet) <- gsub("Acc","Accelerometer",names(reducedSet))
names(reducedSet) <- gsub("tBody","TimeBody",names(reducedSet))
names(reducedSet) <- gsub("^f","Frequency",names(reducedSet))
names(reducedSet) <- gsub("^t","Time",names(reducedSet))
names(reducedSet) <- gsub("Gyro","Gyroscrope",names(reducedSet))
names(reducedSet) <- gsub("BodyBody","Body",names(reducedSet))
names(reducedSet) <- gsub("Mag","Magnitude",names(reducedSet))
names(reducedSet) <- gsub("mean","Mean",names(reducedSet))
names(reducedSet) <- gsub("^anglet","angleTime",names(reducedSet))


#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Final_data <- reducedSet %>% group_by(subject,activity) %>% summarise_all(mean)

write.table(Final_data, "./Data/Final_data.txt", row.name=FALSE)
