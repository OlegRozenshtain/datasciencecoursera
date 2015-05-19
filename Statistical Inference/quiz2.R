quiz2<-function()
{
    # Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are
    # normally distributed with a mean of 80 (mm Hg) and a standard deviation of
    # 10. About what is the probability that a random 35-44 year old has a DBP
    # less than 70?
    ex2<-pnorm(q = 70, mean = 80, sd = 10)
    
    # Brain volume for adult women is normally distributed with a mean of about
    # 1,100 cc for women with a standard deviation of 75 cc. What brain volume
    # represents the 95th percentile?
    ex3<-qnorm(p = 0.95, mean = 1100, sd = 75)
    
    # Refer to the previous question. Brain volume for adult women is about
    # 1,100 cc for women with a standard deviation of 75 cc. Consider the sample
    # mean of 100 random adult women from this population. What is the 95th
    # percentile of the distribution of that sample mean?
    ex4<-qnorm(p = 0.95, mean = 1100, sd = 75/sqrt(100)) # according to CLT
    
    # You flip a fair coin 5 times, about what's the probability of getting 4 or
    # 5 heads?
    ex5<-pbinom(q = 3, size = 5, prob = 0.5, lower.tail = FALSE)
    
    # The number of people showing up at a bus stop is assumed to be Poisson
    # with a mean of 5 people per hour. You watch the bus stop for 3 hours.
    # About what's the probability of viewing 10 or fewer people?
    ex8<-ppois(q = 10, lambda = 5*3)
    
    list(ex2=ex2, ex3=ex3, ex4=ex4, ex5=ex5, ex8=ex8)
}