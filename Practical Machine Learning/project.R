project<-function()
{
    # load activity execution quality training set
    executionQualityTainingData <- read.table(file = "pml-training.csv", sep = ",", 
                                              header = TRUE, comment.char = "",
                                              na.strings = c("NA", "#DIV/0!"),
                                              colClasses = c("integer", "factor", 
                                                             "integer", "integer", 
                                                             "factor", "factor", 
                                                             "integer", 
                                                             rep("numeric", 38*4), 
                                                             "factor"))
    
    # count NA values in each column
    naCount<-sapply(executionQualityTainingData, 
                    function(y) sum(length(which(is.na(y)))))
    # remove columns with majority NA values
    removeColumns <- naCount < 19000
    executionQualityTainingDataClean <-
                                executionQualityTainingData[ , removeColumns]
    # remove general information columns
    executionQualityTainingDataClean <- subset(executionQualityTainingDataClean,
                                               select = -c(X, user_name, 
                                                           raw_timestamp_part_1,
                                                           raw_timestamp_part_2,
                                                           cvtd_timestamp,
                                                           new_window,
                                                           num_window))
    
    # make sure no more NA values exist
    # sum(is.na(executionQualityTainingDataClean))
    
    library(caret)
    preProc <- preProcess(executionQualityTainingDataClean[ , -53], method = "pca")
    # transform parameters to principal components
    executionQualityTainingDataCleanPC <-
                    predict(preProc, executionQualityTainingDataClean[ , -53])
    
    library(doParallel)
    registerDoParallel(cores = 2)
    
    set.seed(1209)
    # Random Forest
    rfModelFit <- train(executionQualityTainingDataClean$classe ~ ., method = "rf", 
                        data = executionQualityTainingDataCleanPC)
    saveRDS(object = rfModelFit, file = "rfModelFit.rds")
    
    set.seed(1209)
    # Stochastic Gradient Boosting (boosting with trees)
    gbmModelFit <- train(executionQualityTainingDataClean$classe ~ ., method = "gbm",
                         trControl = trainControl(method = "cv"), 
                         data = executionQualityTainingDataCleanPC, verbose = FALSE)
    saveRDS(object = gbmModelFit, file = "gbmModelFit.rds")
    
    set.seed(1209)
    # Linear Discriminant Analysis
    ldaModelFit <- train(executionQualityTainingDataClean$classe ~ ., method = "lda",
                         trControl = trainControl(method = "cv"),
                         data = executionQualityTainingDataCleanPC)
    saveRDS(object = ldaModelFit, file = "ldaModelFit.rds")
    
    set.seed(1209)
    # Quadratic Discriminant Analysis
    qdaModelFit <- train(executionQualityTainingDataClean$classe ~ ., method = "qda",
                         trControl = trainControl(method = "cv"),
                         data = executionQualityTainingDataCleanPC)
    saveRDS(object = qdaModelFit, file = "qdaModelFit.rds")
    
    set.seed(1209)
    # Naive Bayes
    nbModelFit <- train(executionQualityTainingDataClean$classe ~ ., method = "nb",
                        trControl = trainControl(method = "cv"),
                        data = executionQualityTainingDataCleanPC)
    saveRDS(object = nbModelFit, file = "nbModelFit.rds")
    
#     max(rfModelFit$results$Accuracy)
#     max(gbmModelFit$results$Accuracy)
#     max(ldaModelFit$results$Accuracy)
#     max(qdaModelFit$results$Accuracy)
#     max(nbModelFit$results$Accuracy)
    
    # rfModelFit

    # load activity execution quality testing set
    executionQualityTestingData <- read.table(file = "pml-testing.csv", sep = ",", 
                                              header = TRUE, comment.char = "",
                                              na.strings = c("NA", "#DIV/0!"),
                                              colClasses = c("integer", "factor", 
                                                             "integer", "integer",
                                                             "factor", "factor",
                                                             "integer",
                                                             rep("numeric", 38*4),
                                                             "integer"))
    # remove columns with majority NA values
    executionQualityTestingDataClean <-
                                executionQualityTestingData[ , removeColumns]
    # remove general information columns
    executionQualityTestingDataClean <- subset(executionQualityTestingDataClean,
                                               select = -c(X, user_name, 
                                                           raw_timestamp_part_1,
                                                           raw_timestamp_part_2,
                                                           cvtd_timestamp,
                                                           new_window,
                                                           num_window))
#     sum(is.na(executionQualityTestingDataClean))

    # transform parameters to principal components
    executionQualityTestingDataCleanPC <-
                    predict(preProc, executionQualityTestingDataClean[ , -53])

    data.frame(problem_id = executionQualityTestingDataClean$problem_id, 
               classe =  predict(rfModelFit, executionQualityTestingDataCleanPC))
}