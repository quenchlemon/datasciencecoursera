#Load Packages
library(plyr)
library(data.table)

run_analysis <- function(){
  # Load training data
  training_data <- read.table("train/X_train.txt")
  dim(training_data) # 7352, 561
  training_label <- read.table("train/y_train.txt")
  table(training_label)
  training_subject <- read.table("train/subject_train.txt")
  # Load testing data
  testing_data <- read.table("test/X_test.txt")
  dim(testing_data) #2947, 561
  testing_label <- read.table("test/y_test.txt")
  table(testing_label)
  testing_subject <- read.table("test/subject_test.txt")
  ## Joining datasets
  data_join <- rbind(training_data, testing_data); dim(data_join) #10299, 561
  label_join <- rbind(training_label, testing_label); dim(label_join) #10299, 1
  subject_join <- rbind(training_subject, testing_subject); dim(subject_join) #10299, 1
  
  # Extracts only measurements on the mean and standard deviation for each measurement
  dataset_features <- read.table("features.txt", col.names=c("featureId", "featureLabel"))
  dataset_activities <- read.table("activity_labels.txt", col.names=c("activityId", "activityLabel"))
  dataset_activities$activityLabel <- gsub("_", "", as.character(dataset_activities$activityLabel))
  includedFeatures <- grep("-mean\\(\\)|-std\\(\\)", dataset_features$featureLabel)
  
  ## Joining datasets
  subject <- rbind(testing_subject, training_subject)
  names(subject) <- "subjectId"
  data <- rbind(testing_data, training_data)
  data <- data[, includedFeatures]
  names(data) <- gsub("\\(|\\)", "", dataset_features$featureLabel[includedFeatures])
  label <- rbind(testing_label, training_label)
  names(label) = "activityId"
  activity <- merge(label, dataset_activities, by="activityId")$activityLabel
  
  # merge data frames of different columns to form one data table
  data <- cbind(subject, data, activity)
  write.table(data, "merged_tidy_data.txt", row.names = FALSE)
  
  # create dataset grouped by subject and activity after evaluating standard deviation and means 
  dataDT <- data.table(data)
  calculatedData<- dataDT[, lapply(.SD, mean), by=c("subjectId", "activity")]
  write.table(calculatedData, "calculated_tidy_data.txt", row.names = FALSE)
}

run_analysis() #runs function to generate tidy data
