quiz3<-function()
{
    # Consider the mtcars data set. Fit a model with mpg as the outcome that
    # includes number of cylinders as a factor variable and weight as
    # confounder. Give the adjusted estimate for the expected change in mpg
    # comparing 8 cylinders to 4.
    data(mtcars)
    # third line is cyl8 parameter, the first column is the Estimate.
    # cyl4 is the reference.
    ex1<-summary(lm(mpg ~ factor(cyl) + wt, data = mtcars))$coef[3][1]
    
    # Consider the mtcars data set. Fit a model with mpg as the outcome that
    # considers number of cylinders as a factor variable and weight as
    # confounder. Now fit a second model with mpg as the outcome model that
    # considers the interaction between number of cylinders (as a factor
    # variable) and weight. Give the P-value for the likelihood ratio test
    # comparing the two models and suggest a model using 0.05 as a type I error
    # rate significance benchmark.
    data(mtcars)
    fit1<-lm(mpg ~ factor(cyl) + wt, data = mtcars)
    fit2<-lm(mpg ~ factor(cyl) + wt + factor(cyl)*wt, data = mtcars)
    ex3<-anova(fit1,fit2)
    
    # Consider the following data set:
    # x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
    # y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
    # Give the hat diagonal for the most influential point
    x<-c(0.586, 0.166, -0.042, -0.614, 11.72)
    y< c(0.549, -0.026, -0.127, -0.751, 1.344)
    ex5<-max(hatvalues(lm(y~x)))
    
    # Consider the following data set
    # x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
    # y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
    # Give the slope dfbeta for the point with the highest hat value.
    x<-c(0.586, 0.166, -0.042, -0.614, 11.72)
    y< c(0.549, -0.026, -0.127, -0.751, 1.344)
    ex6<-dfbetas(lm(y~x))
}