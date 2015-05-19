quiz3<-function()
{
    # In a population of interest, a sample of 9 men yielded a sample average
    # brain volume of 1,100cc and a standard deviation of 30cc. What is a 95%
    # Student's T confidence interval for the mean brain volume in this new
    # population?
    ex1<-1100 + c(-1,1)*qt(0.975, 8) * 30/sqrt(9)
    
    # A diet pill is given to 9 subjects over six weeks. The average difference
    # in weight (follow up - baseline) is -2 pounds. What would the standard
    # deviation of the difference in weight have to be for the upper endpoint of
    # the 95% T confidence interval to touch 0?
    ex2<-2/(qt(0.975, 8)/sqrt(9))   # solve: -2 + qt(0.975, 8) * s/sqrt(9) = 0
    
    # In a study of emergency room waiting times, investigators consider a new
    # and the standard triage systems. To test the systems, administrators
    # selected 20 nights and randomly assigned the new triage system to be used
    # on 10 nights and the standard system on the remaining 10 nights. They
    # calculated the nightly median waiting time (MWT) to see a physician. The
    # average MWT for the new system was 3 hours with a variance of 0.60 while
    # the average MWT for the old system was 5 hours with a variance of 0.68.
    # Consider the 95% confidence interval estimate for the differences of the
    # mean MWT associated with the new system. Assume a constant variance. What
    # is the interval? Subtract in this order (New System - Old System).
    sp<-sqrt((9*0.6 + 9*0.68) / 18)     # pooled standard deviation
    ex4<-3 - 5 + c(-1,1)*qt(0.975, 18) * sp*sqrt(1/10 + 1/10)
    
    # To further test the hospital triage system, administrators selected 200
    # nights and randomly assigned a new triage system to be used on 100 nights
    # and a standard system on the remaining 100 nights. They calculated the
    # nightly median waiting time (MWT) to see a physician. The average MWT for
    # the new system was 4 hours with a standard deviation of 0.5 hours while
    # the average MWT for the old system was 6 hours with a standard deviation
    # of 2 hours. Consider the hypothesis of a decrease in the mean MWT
    # associated with the new treatment. What does the 95% independent group
    # confidence interval with unequal variances suggest vis a vis this
    # hypothesis? (Because there's so many observations per group, just use the
    # Z quantile instead of the T.)
    se<-sqrt(0.5^2/100 + 2^2/100)   # standard error
    ex6<-6 - 4 + c(-1,1)*qnorm(0.975, mean = 2, sd = se) * se
    
    # Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill
    # and a placebo. Subjects’ body mass indices (BMIs) were measured at a
    # baseline and again after having received the treatment or placebo for four
    # weeks. The average difference from follow-up to the baseline (followup -
    # baseline) was ???3 kg/m2 for the treated group and 1 kg/m2 for the placebo
    # group. The corresponding standard deviations of the differences was 1.5
    # kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does
    # the change in BMI over the four week period appear to differ between the
    # treated and placebo groups? Assuming normality of the underlying data and
    # a common population variance, calculate the relevant *90%* t confidence
    # interval. Subtract in the order of (Treated - Placebo) with the smaller
    # (more negative) number first.
    sp<-sqrt((8*1.5^2 + 8*1.8^2) / 16)     # pooled standard deviation
    ex7<- -3 - 1 + c(-1,1)*qt(0.95, 16) * sp*sqrt(1/9 + 1/9)
    
    
    list(ex1=ex1, ex2=ex2, ex4=ex4, ex6=ex6, ex7=ex7)
}