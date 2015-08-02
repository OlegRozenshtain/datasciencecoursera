quiz4<-function()
{
    # Load the vowel.train and vowel.test data sets:
    library(ElemStatLearn)
    data(vowel.train)
    data(vowel.test) 
    # Set the variable y to be a factor variable in both the training and test
    # set. Then set the seed to 33833. Fit (1) a random forest predictor
    # relating the factor variable y to the remaining variables and (2) a
    # boosted predictor using the "gbm" method. Fit these both with the train()
    # command in the caret package.
    # What are the accuracies for the two approaches on the test data set? What
    # is the accuracy among the test set samples where the two methods agree?
    vowel.train$y <- as.factor(vowel.train$y)
    vowel.test$y <- as.factor(vowel.test$y)
    set.seed(33833)
    library(caret)
    mod.rf <- train(y ~ ., method = "rf", data = vowel.train)
    mod.gbm <- train(y ~ ., method = "gbm", data = vowel.train, verbose = FALSE)
    pred.rf <- predict(mod.rf, vowel.test)
    pred.gbm <- predict(mod.gbm, vowel.test)
    modelsAgree <- pred.rf == pred.gbm
    
    ex1 <- list(
        rfAccuracy = confusionMatrix(pred.rf, vowel.test$y)$overall[["Accuracy"]],
        gbmAccuracy = confusionMatrix(pred.gbm, vowel.test$y)$overall[["Accuracy"]],
        agreementAccuracy = confusionMatrix(
                                pred.rf[modelsAgree], 
                                vowel.test$y[modelsAgree])$overall[["Accuracy"]])
    
    # Load the Alzheimer's data using the following commands
    library(caret)
    library(gbm)
    set.seed(3433)
    library(AppliedPredictiveModeling)
    data(AlzheimerDisease)
    adData = data.frame(diagnosis,predictors)
    inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
    training = adData[ inTrain,]
    testing = adData[-inTrain,]
    # Set the seed to 62433 and predict diagnosis with all the other variables
    # using a random forest ("rf"), boosted trees ("gbm") and linear
    # discriminant analysis ("lda") model. Stack the predictions together using
    # random forests ("rf"). What is the resulting accuracy on the test set? Is
    # it better or worse than each of the individual predictions?
    set.seed(62433)
    mod.rf <- train(diagnosis ~ ., method = "rf", data = training)
    mod.gbm <- train(diagnosis ~ ., method = "gbm", data = training, 
                     verbose = FALSE)
    mod.lda <- train(diagnosis ~ ., method = "lda", data = training)
    predTrain.rf <- predict(mod.rf, training)
    predTrain.gbm <- predict(mod.gbm, training)
    predTrain.lda <- predict(mod.lda, training)
    predTrainDF <- data.frame(pred.rf = predTrain.rf, pred.gbm = predTrain.gbm, 
                              pred.lda = predTrain.lda, 
                              diagnosis = training$diagnosis)
    combModFit <- train(diagnosis ~ ., method = "rf", data = predTrainDF)
    predTest.rf <- predict(mod.rf, testing)
    predTest.gbm <- predict(mod.gbm, testing)
    predTest.lda <- predict(mod.lda, testing)
    predTestDF <- data.frame(pred.rf = predTest.rf, pred.gbm = predTest.gbm, 
                             pred.lda = predTest.lda, 
                             diagnosis = testing$diagnosis)
    predComb <- predict(combModFit, predTestDF)
    ex2 <- list(
        rfAccuracy = confusionMatrix(predTest.rf, testing$diagnosis)$overall[["Accuracy"]],
        gbmAccuracy = confusionMatrix(predTest.gbm, testing$diagnosis)$overall[["Accuracy"]],
        ldaAccuracy = confusionMatrix(predTest.lda, testing$diagnosis)$overall[["Accuracy"]],
        combAccuracy = confusionMatrix(predComb, predTestDF$diagnosis)$overall[["Accuracy"]])
    
    # Load the concrete data with the commands:
    set.seed(3523)
    library(AppliedPredictiveModeling)
    data(concrete)
    inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
    training = concrete[ inTrain,]
    testing = concrete[-inTrain,]
    # Set the seed to 233 and fit a lasso model to predict Compressive Strength.
    # Which variable is the last coefficient to be set to zero as the penalty
    # increases? (Hint: it may be useful to look up ?plot.enet).
    library(caret)
    library(elasticnet)
    set.seed(233)
    mod.lasso <- enet(x = as.matrix(subset(training, 
                                           select = -c(CompressiveStrength))), 
                      y = training$CompressiveStrength, lambda = 0, trace = TRUE)
    plot.enet(x = mod.lasso, xvar = "penalty", use.color = TRUE)
    
    # Load the data on the number of visitors to the instructors blogUsing the 
    # commands:
    library(lubridate)  # For year() function below
    dat = read.csv("gaData.csv")
    training = dat[year(dat$date) < 2012,]
    testing = dat[(year(dat$date)) > 2011,]
    tstrain = ts(training$visitsTumblr)
    # Fit a model using the bats() function in the forecast package to the
    # training time series. Then forecast this model for the remaining time
    # points. For how many of the testing points is the true value within the
    # 95% prediction interval bounds?
    library(forecast)
    mod.bats <- bats(tstrain)
    fcast <- forecast(mod.bats, level = 95, h = length(testing$visitsTumblr))
    ex4 <- 1 - sum(testing$visitsTumblr > fcast$upper) / length(testing$visitsTumblr)
    
    # Load the concrete data with the commands:
    set.seed(3523)
    library(AppliedPredictiveModeling)
    data(concrete)
    inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
    training = concrete[ inTrain,]
    testing = concrete[-inTrain,]
    # Set the seed to 325 and fit a support vector machine using the e1071
    # package to predict Compressive Strength using the default settings.
    # Predict on the testing set. What is the RMSE?
    set.seed(325)
    library(e1071)
    mod.svm <- svm(CompressiveStrength ~ . , data = training)
    ex5 <- accuracy(predict(mod.svnm, testing), testing$CompressiveStrength)[2]
    
    
    list(ex1=ex1, ex2=ex2, ex4=ex4, ex5=ex5)
}