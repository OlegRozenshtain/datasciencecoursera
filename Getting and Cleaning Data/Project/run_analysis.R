# The purpose of this project is to demonstrate your ability to collect, work
# with, and clean a data set. The goal is to prepare tidy data that can be used
# for later analysis.
run_analysis<-function()
{
    # read general descriptive tables of labels
    activityLabels<-read.table("./UCI HAR Dataset/activity_labels.txt",
                               comment.char = "", col.names = c("activityCode", 
                                                                "activityName"))
    features<-read.table("./UCI HAR Dataset/features.txt", comment.char = "", 
                         colClasses = c("integer", "character"), 
                         col.names = c("featureIndex", "featureName"))
    
    # read feature vector, subject identifier and type of activety from test set
    testSet<-read.table("./UCI HAR Dataset/test/X_test.txt", comment.char = "", 
                        colClasses = rep("double", 561))
    testSubjectId<-read.table("./UCI HAR Dataset/test/subject_test.txt", 
                              comment.char = "", colClasses = "integer")
    testActivetyType<-read.table("./UCI HAR Dataset/test/y_test.txt",
                                 comment.char = "", colClasses = "factor")
    
    # read feature vector, subject identifier and type of activety from training 
    # set
    trainSet<-read.table("./UCI HAR Dataset/train/X_train.txt", 
                         comment.char = "", colClasses = rep("double", 561))
    trainSubjectId<-read.table("./UCI HAR Dataset/train/subject_train.txt",
                               comment.char = "", colClasses = "integer")
    trainActivetyType<-read.table("./UCI HAR Dataset/train/y_train.txt",
                                  comment.char = "", colClasses = "factor")
    
    # add subject identifier and type of activety entry to the resulting feature 
    # vector, then merge the test and training sets.
    testFullData<-cbind(testSubjectId, testActivetyType, testSet)
    trainFullData<-cbind(trainSubjectId, trainActivetyType, trainSet)
    fullData<-rbind(testFullData, trainFullData)
    
    # calculate a logical vector indicating measurements of mean and standard 
    # deviation. it is known from the features_info.txt file that no measurement
    # name has both "mean()" and "std()" key words
    logicalRelevantmeasurements<-
                        (grepl("mean()", features$featureName, fixed = TRUE)) | 
                        (grepl("std()", features$featureName, fixed = TRUE))
    # get the corresponding relevant indexes 
    indexRelevantmeasurements<-features$featureIndex[logicalRelevantmeasurements]
    # add 2 first indexes representing subject identifier and type of activety
    indexRelevantmeasurements<-c(1, 2, indexRelevantmeasurements+2)
    
    # keep only relevant columns from the full data set
    partialMeasurementsData<-fullData[, indexRelevantmeasurements]
    
    # use descriptive activity names to name the activities types in the data set
    levels(partialMeasurementsData[,2])<-activityLabels$activityName
    
    # label the data set with descriptive variable names
    colnames(partialMeasurementsData)<-
                        c("subjectId", "activityType", 
                            features$featureName[logicalRelevantmeasurements])
    
    # aggregate tidy data set with the average of each variable for each
    # activity and each subject
    tidyData<-aggregate(. ~ subjectId + activityType, 
                        data = partialMeasurementsData, mean)
    
    # output results
    write.table(tidyData, file = "GettingAndCleaningData_CourseProject.txt", 
                row.names = FALSE)
}