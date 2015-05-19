quiz4<-function()
{
    # A pharmaceutical company is interested in testing a potential blood 
    # pressure lowering medication. Their first examination considers only 
    # subjects that received the medication at baseline then two weeks later. 
    # Consider testing the hypothesis that there was a mean reduction in blood
    # pressure? Give the P-value for the associated two sided T test.
    baseline<-c(140, 138, 150, 148, 135)
    week2<- c(132, 135, 151, 146, 130)
    ex1<-t.test(baseline, week2, alternative = "two.sided", paired = TRUE)$p.value
    
    # A sample of 9 men yielded a sample average brain volume of 1,100cc and a 
    # standard deviation of 30cc. What is the complete set of values of mu0 that
    # a test of H0:mu=mu0 would fail to reject the null hypothesis in a two
    # sided 5% Students t-test?
    ex2<-1100 + c(-1,1)*qt(p = 0.975, df = 9-1)*30/sqrt(9)
    
    # Researchers conducted a blind taste test of Coke versus Pepsi. Each of
    # four people was asked which of two blinded drinks given in random order
    # that they preferred. The data was such that 3 of the 4 people chose Coke.
    # Assuming that this sample is representative, report a P-value for a test
    # of the hypothesis that Coke is preferred to Pepsi using a one sided exact
    # test.
    ex3<-pbinom(q = 2, size = 4, prob = 0.5, lower.tail = FALSE)
    
    # Infection rates at a hospital above 1 infection per 100 person days at
    # risk are believed to be too high and are used as a benchmark. A hospital
    # that had previously been above the benchmark recently had 10 infections
    # over the last 1,787 person days at risk. About what is the one sided
    # P-value for the relevant test of whether the hospital is *below* the
    # standard?
    ex4<-ppois(q = 10, lambda = 1*1787/100)
    
    # Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill
    # and a placebo. Subjects' body mass indices (BMIs) were measured at a
    # baseline and again after having received the treatment or placebo for four
    # weeks. The average difference from followup to the baseline (followup -
    # baseline) was ???3 kg/m2 for the treated group and 1 kg/m2 for the placebo
    # group. The corresponding standard deviations of the differences was 1.5
    # kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does
    # the change in BMI appear to differ between the treated and placebo groups?
    # Assuming normality of the underlying data and a common population
    # variance, give a pvalue for a two sided t test.
    sp<-sqrt((8*1.5^2 + 8*1.8^2) / 16)
    ex5<- -3 - 1 + c(-1,1)*qt(0.995, 16) * sp*sqrt(1/9 + 1/9)
    # answer: less than 0.01
    
    
    # Researchers would like to conduct a study of 100 healthy adults to detect
    # a four year mean brain volume loss of .01 mm3. Assume that the standard
    # deviation of four year volume loss in this population is .04 mm3. About
    # what would be the power of the study for a 5% one sided test versus a null
    # hypothesis of no volume loss?
    ex7<-power.t.test(n = 100, delta = 0.01, sd = 0.04, sig.level = 0.05, 
                      type = "paired", alternative = "one.sided")$power
    
    # Researchers would like to conduct a study of n healthy adults to detect a
    # four year mean brain volume loss of .01 mm3. Assume that the standard
    # deviation of four year volume loss in this population is .04 mm3. About
    # what would be the value of n needded for 90% power of type one error rate
    # of 5% one sided test versus a null hypothesis of no volume loss?
    ex8<-power.t.test(delta = 0.01, sd = 0.04, sig.level = 0.05, power = 0.9,
                      alternative = "one.sided", type = "paired")$n
    
    list(ex1=ex1, ex2=ex2, ex3=ex3, ex4=ex4, ex5=ex5, ex7=ex7, ex8=ex8)
}