quiz2<-function()
{
    # Consider the following data with x as the predictor and y as as the outcome.
    # x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
    # y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
    # Give a P-value for the two sided hypothesis test of whether beta1 from a 
    # linear regression model is 0 or not.
    x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
    y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
    ex1<-summary(lm(y~x))$coefficients
    
    # Consider the previous problem, give the estimate of the residual standard
    # deviation.
    x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
    y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
    ex2<-summary(lm(y~x))$coefficients
    
    # In the mtcars data set, fit a linear regression model of weight
    # (predictor) on mpg (outcome). Get a 95% confidence interval for the
    # expected mpg at the average weight. What is the lower endpoint?
    data(mtcars)
    fit<-lm(mpg~wt, data = mtcars)
    # default 95% confidence interval
    ex3<-predict(fit, newdata = data.frame(wt=mean(mtcars$wt)), 
                 interval = "confidence")
    
    # Consider again the mtcars data set and a linear regression model with mpg
    # as predicted by weight (1,000 lbs). A new car is coming weighing 3000
    # pounds. Construct a 95% prediction interval for its mpg. What is the upper
    # endpoint?
    data(mtcars)
    fit<-lm(mpg~wt, data = mtcars)
    # default 95% confidence interval
    ex5<-predict(fit, newdata = data.frame(wt = 3), 
                 interval = "prediction")
    
    # Consider again the mtcars data set and a linear regression model with mpg
    # as predicted by weight (in 1,000 lbs). A "short" ton is defined as 2,000
    # lbs. Construct a 95% confidence interval for the expected change in mpg
    # per 1 short ton increase in weight. Give the lower endpoint.
    data(mtcars)
    fit<-lm(mpg~wt, data = mtcars)
    sumCoef<-summary(fit)$coefficients
    ex6<-(sumCoef[2,1] - qt(0.975, df = fit$df) * sumCoef[2,2]) * 2
    
    list(ex1=ex1, ex2=ex2, ex3=ex3, ex5=ex5, ex6=ex6)
}
