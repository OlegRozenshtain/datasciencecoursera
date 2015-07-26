quiz4<-function()
{
    # Consider the space shuttle data ?shuttle in the MASS library. Consider
    # modeling the use of the autolander as the outcome (variable name use). Fit
    # a logistic regression model with autolander (variable auto) use (labeled
    # as "auto" 1) versus not (0) as predicted by wind sign (variable wind).
    # Give the estimated odds ratio for autolander use comparing head winds,
    # labeled as "head" in the variable headwind (numerator) to tail winds
    # (denominator).
    library("MASS")
    data(shuttle)
    numericUse<-1*(shuttle$use == "auto")
    wind2<-relevel(shuttle$wind, "tail")
    ex1<-exp(summary(glm(numericUse ~ wind2, family = "binomial"))$coef[2][1])
    
    # Consider the previous problem. Give the estimated odds ratio for
    # autolander use comparing head winds (numerator) to tail winds
    # (denominator) adjusting for wind strength from the variable magn.
    library("MASS")
    data(shuttle)
    numericUse<-1*(shuttle$use == "auto")
    wind2<-relevel(shuttle$wind, "tail")
    ex2<-exp(summary(glm(numericUse ~ wind2 + shuttle$magn, 
                         family = "binomial"))$coef[2][1])
    
    # Consider the insect spray data InsectSprays. Fit a Poisson model using
    # spray as a factor level. Report the estimated relative rate comapring
    # spray A (numerator) to spray B (denominator).
    data(InsectSprays)
    spray2<-relevel(InsectSprays$spray, "B")
    ex4<-exp(summary(glm(InsectSprays$count ~ spray2, 
                         family = "poisson"))$coef[2][1])
    
    # Consider the data:
    # x <- -5:5
    # y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
    # Using a knot point at 0, fit a linear model that looks like a hockey stick
    # with two lines meeting at x=0. Include an intercept term, x and the knot
    # point term. What is the estimated slope of the line after 0?
    x<- -5:5
    y<-c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
    splineTerms<-(x > 0)*x
    xMat<-cbind(x, splineTerms)
    # The statistical model is: Y = b0 + b1X + b2(X - C) + e
    # where b0 is the intercept,
    # b1 is the slope before the change point C,
    # and b2 is the DIFFERENCE in slope after the change point.
    # The slope after the change point is b1 + b2.
    ex6<-summary(lm(y ~ xMat))$coef[3][1] + summary(lm(y ~ xMat))$coef[2][1]
    
    list(ex1=ex1, ex2=ex2, ex4=ex4, ex6=ex6)
}