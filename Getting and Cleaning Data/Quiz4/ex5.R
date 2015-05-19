# You can use the quantmod  package to get historical stock prices for publicly 
# traded companies on the NASDAQ and NYSE. Use the following code to download 
# data on Amazon's stock price and get the times the data was sampled. 
# How many values were collected in 2012? How many values were collected on 
# Mondays in 2012?
ex5<-function()
{
    library(quantmod)
    amzn = getSymbols("AMZN",auto.assign=FALSE)
    sampleTimes = index(amzn) 
    
    collected2012<-format(sampleTimes, "%Y") == 2012

    library(lubridate)
    
    c(sum(collected2012),
     sum(wday(sampleTimes[collected2012]) == 2))
}