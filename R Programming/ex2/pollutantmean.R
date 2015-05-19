# calculates the mean of a pollutant (sulfate or nitrate) across a specified 
# list of monitors. The function 'pollutantmean' takes three arguments: 
# 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 
# 'pollutantmean' reads that monitors' particulate matter data from the 
# directory specified in the 'directory' argument and returns the mean of the 
# pollutant across all of the monitors, ignoring any missing values coded as NA.
pollutantmean <- function(directory, pollutant, id = 1:332)
{
    pollutantSum = 0    # sum of all pallutant's values across all monitors
    obsNum = 0          # number of all pallutant's values across all monitors
    
    for (i in 1:length(id))
    {
        # create filename "directory/***.csv". *** is a number with 3 digits
        # (set leading zeros in case of less than 3 digits)
        fileName <- paste(directory, formatC(id[i], width=3, flag=0), sep = "/")
        fileName <- paste(fileName, "csv", sep = ".")
        
        # read data frame from file to data
        data<-read.csv(fileName, header = TRUE, comment.char = "")
        
        # add sum of pollutant column in data (ignoring NA's) to the sum of
        # pollutant columns in previous monitors.  
        pollutantSum = pollutantSum + sum(data[ ,pollutant], na.rm = TRUE)
        
        # add the number of observations in column pollutant in data that are 
        # not NA's to the observations number in previou monitors. 
        index = !is.na(data[ ,pollutant])
        obsNum = obsNum + length(data[ ,pollutant][index])
    }
    
    # calculate the average
    pollutantSum / obsNum
}