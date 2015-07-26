quiz1<-function()
{
    # Consider the data set x <- c(0.18, -1.54, 0.42, 0.95)
    # And weights given by w <- c(2, 1, 3, 1)
    # Give the value of mu that minimizes the least squares equation 
    # sum(w_i*(x_i-mu)^2)
    x<-c(0.18, -1.54, 0.42, 0.95)
    w<-c(2, 1, 3, 1)
    # the weights can be interpreted as number of occurrence of each element of x
    ex1<-mean(rep(x,w))
    
    # Consider the following data set
    # x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
    # y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
    # Fit the regression through the origin and get the slope treating y as the
    # outcome and x as the regressor. (Hint, do not center the data since we
    # want regression through the origin, not through the means of the data.)
    x<-c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
    y<-c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
    ex2<-coef(lm(y~x - 1))
    
    # Do data(mtcars) from the datasets package and fit the regression model
    # with mpg as the outcome and weight as the predictor. Give the slope
    # coefficient.
    library(datasets)
    data(mtcars)
    ex3<-coef(lm(mtcars$mpg~mtcars$wt))[2]
    
    # Consider the data given by the following x<-c(8.58, 10.46, 9.01, 9.64, 8.86)
    # What is the value of the first measurement if x were normalized (to have
    # mean 0 and variance 1)?
    x<-c(8.58, 10.46, 9.01, 9.64, 8.86)
    m<-mean(x)
    s<-sd(x)
    xnormalized<-(x-m)/s
    ex6<-xnormalized[1]
    
    # Consider the following data set (used above as well). 
    # x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
    # y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
    # What is the intercept for fitting the model with x as the predictor and y
    # as the outcome?
    x<-c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
    y<-c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
    ex7<-coef(lm(y~x))[1]
    
    list(ex1=ex1, ex2=ex2, ex3=ex3, ex6=ex6, ex7=ex7)
}