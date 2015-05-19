investigateExponential<-function()
{
    library(ggplot2)
    
    # set exponential sample simulations parameters. 
    # simulate 1000 different samples of size 40, out of exponential 
    # distribution with rate 0.2
    simulationNumber<-1000
    lambda<-0.2
    sampleSize<-40    
    
    set.seed(12)
    # simulate the samples. the result is a matrix with each sample as a column
    sampleVector<-replicate(simulationNumber, rexp(n = sampleSize, rate = lambda))
    # calculate the mean for each sample
    sampleMeanVector<-colMeans(sampleVector)
    
    # set title and axis themes for all future plots
    plotTheme<-theme(plot.title = element_text(size = 18, face = "bold", vjust = 2),
                     axis.title = element_text(size = 14), 
                     axis.text = element_text(size = 12))
    
    # convert to a data frame, because ggplot works only with data frames
    dfSampleMeanVector<-data.frame(sampleMeanVector)
    # create a histogram out of samples means
    pMean<-ggplot(dfSampleMeanVector, aes(x = sampleMeanVector)) +
           labs(title = "sample mean histogram", x = "sample mean") + 
           geom_histogram(binwidth = 0.2, colour="black", fill="cadetblue1") +
           plotTheme
    
    # add vertical lines to the histogram to show the theoretical mean of the
    # exponential distribution (=1/lambda) and the sample mean
    pMeanVerticalLines<-pMean + 
                geom_vline(xintercept = c(1/lambda, mean(sampleMeanVector)) , 
                           colour = c("red", "darkgreen"), linetype = "longdash")
    
    # add a smooth density estimate curve to the histogram to show gaussian form.
    # add a smooth normal density estimate curve to show similarity
    pMeanDensity<-pMean + aes(y = ..density..) + 
        geom_density(colour = "darkgreen", size = 1.5) + 
        geom_density(data = data.frame(x = rnorm(n = simulationNumber, 
                                                 mean = 1/lambda, 
                                                 sd = (1/lambda)/sqrt(sampleSize))),
                     colour = "red", size = 1.5)
    
    # create a data frame with a 1000 iids exponntially distributed with lambda=0.2
    dfExponentialVector<-data.frame(exponentialVector = rexp(n = simulationNumber,
                                                             rate = lambda))
    # create a histogram of the exponntially distributed iids and add a smooth 
    # density estimate curve to the histogram to show exponential form instead
    # of gaussian
    pExponentialDensity<-
        ggplot(dfExponentialVector, aes(x = exponentialVector, y = ..density..)) +
        labs(title = "exponential iid histogram", x = "exponential iid, lambda = 0.2") + 
        geom_histogram(binwidth = 1, colour="black", fill="cadetblue1") +
        geom_density(size = 1.5) + plotTheme
    
    
    # save all plots to png files
    png("sample_mean_histogram.png", width = 960)
    print(pMeanVerticalLines) 
    dev.off()
    
    png("sample_mean_histogram_zoomin.png", width = 600)
    print(pMeanVerticalLines + coord_cartesian(xlim = 1/lambda + c(1,-1)*0.5))
    dev.off()
    
    png("sample_mean_density.png", width = 960)
    print(pMeanDensity + ylab("density")) 
    dev.off()
    
    png("exponential_density.png", width = 960)
    print(pExponentialDensity + ylab("density")) 
    dev.off()
}