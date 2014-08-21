require(reshape2)

directory <- "UCI HAR Dataset"
fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Main Function to call to run the program
main <- function() {
        downloadZip()                
        #Merge Training and Test sets to one data set        
        merged <- getMerged()
        #Extract measurements on mean and standard deviation for each measurement
        mean_and_std <- getMeanSD(merged)
        #Use Descriptive Activity Names to name activities
        with_activities <- labelActivities(mean_and_std)
        #Appropriately label data set with descriptive variable names
        
        #Write out first tidy data set
        write.table(with_activities, file="tidy.txt")
        #Create second independent tidy data set with average of each variable for each activity and each subject
        average_df <- addActivityAverage(with_activities)
        write.table(average_df, file="tidy_average.txt", row.names=FALSE)
        
        print("Done. Check files tidy.txt and tidy_average.txt")
        
}

#Download Zip
downloadZip <- function() {
        if (!file.exists(directory)) {
                download.file(fileUrl, "projectDataset.zip")
                unzip("projectDataset.zip")
                file.remove("projectDataset.zip")
        }
}
# get the Merged Training and Test data sets
getMerged <- function() {

    # read data files into R data frames
    features <- read.table(file.path(directory, "features.txt"),)
    train_X <- read.table(
        file.path(directory, "train", "X_train.txt"),
        header=FALSE)
    test_X <- read.table(
        file.path(directory, "test", "X_test.txt"),
        header=FALSE)

    train_y <- read.table(
        file.path(directory, "train", "y_train.txt"),
        header=FALSE)
    test_y <- read.table(
        file.path(directory, "test", "y_test.txt"),
        header=FALSE)

    train_subject <- read.table(
        file.path(directory, "train", "subject_train.txt"),
        header=FALSE)
    test_subject <- read.table(
        file.path(directory, "test", "subject_test.txt"),
        header=FALSE)

    merged_X <- rbind(train_X, test_X)
    merged_y <- rbind(train_y, test_y)
    merged_subject <- rbind(train_subject, test_subject)

    # set column names
    colnames(merged_X) <- features[, 2]
    colnames(merged_y) <- c("ActivityId")
    colnames(merged_subject) <- c("SubjectId")

    # merge x and y
    merged <- cbind(merged_X, merged_subject, merged_y)
    merged
}

# subset data frame based on valid columns for mean or standard deviation
getMeanSD <- function(df) {

    valid_columns <- grep("mean|std|SubjectId|ActivityId", names(df), value=TRUE)

    subset(df, select=valid_columns)
}

# replace acc with accelerometer
# replace Gyro with gyroscope 
# use descriptive activity names
labelActivities <- function(df) {
        
    activity_df <- read.table(
        file.path(directory, "activity_labels.txt"), header=FALSE)
        names(activity_df) <- c("id", "Activity")
        names(df) <- gsub("Acc", "Accelerometer", names(df))
        names(df) <- gsub("Gyro", "Gyroscope", names(df))
        with_activities <- merge(df, activity_df, by.x="ActivityId", by.y="id")
        with_activities$ActivityId <- NULL
        with_activities
}

#Get average of each variable for each activity and each subject
addActivityAverage <- function(df) {
        
    ids <- c("SubjectId", "Activity")
    all_except_ids <- Filter(function(x) !(x %in% ids), names(df))
    activities_melt <- melt(df, id=ids, measure.vars=all_except_ids)
    average_df <- dcast(activities_melt, SubjectId + Activity ~ variable, mean)
    average_df
}
