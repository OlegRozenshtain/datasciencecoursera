quiz2<-function()
{
    # Load the Alzheimer's disease data using the commands:
    library(caret)
    library(AppliedPredictiveModeling)
    set.seed(3433)
    data(AlzheimerDisease)
    adData = data.frame(diagnosis,predictors)
    inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
    training = adData[ inTrain,]
    testing = adData[-inTrain,]
    # Find all the predictor variables in the training set that begin with IL.
    # Perform principal components on these variables with the preProcess()
    # function from the caret package. Calculate the number of principal
    # components needed to capture 90% of the variance. How many are there?
    ex3<-preProcess(training[ ,grep(pattern = "^IL", x = names(training))], 
                    method = "pca", thresh = 0.9)$numComp
    
    
    # Load the Alzheimer's disease data using the commands:
    library(caret)
    library(AppliedPredictiveModeling)
    set.seed(3433)
    data(AlzheimerDisease)
    adData = data.frame(diagnosis,predictors)
    inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
    training = adData[ inTrain,]
    testing = adData[-inTrain,]
    # Create a training data set consisting of only the predictors with variable
    # names beginning with IL and the diagnosis. Build two predictive models,
    # one using the predictors as they are and one using PCA with principal
    # components explaining 80% of the variance in the predictors. Use
    # method="glm" in the train function. What is the accuracy of each method in
    # the test set? Which is more accurate?
    training<-data.frame(training$diagnosis, 
                         training[ ,grep(pattern = "^IL", x = names(training))])
    testing<-data.frame(testing$diagnosis, 
                        testing[ ,grep(pattern = "^IL", x = names(testing))])
    
    modelFitNoPCA<-train(training.diagnosis ~ ., method = "glm", data = training)
    accNoPCA<-confusionMatrix(testing$testing.diagnosis, 
                              predict(modelFitNoPCA, testing[, -1]))$overall[1]
    
    # calculate transformation from current variables to PCA's
    preProc<-preProcess(training[ , -1], method = "pca", thresh = 0.8)
    # transform training variables to PCA's
    trainPC<-predict(preProc, training[ , -1])
    # build model using PCA's
    modelFitPCA<-train(training$training.diagnosis ~ ., method = "glm", 
                       data = trainPC)
    # transform testing variables to PCA's (using training based transformation!)
    testPC<-predict(preProc, testing[ , -1])
    accPCA<-confusionMatrix(testing$testing.diagnosis, 
                            predict(modelFitPCA, testPC))$overall[1]
    
    
    list(ex3 = ex3, ex4 = list(accNoPCA = accNoPCA, accPCA = accPCA))
}