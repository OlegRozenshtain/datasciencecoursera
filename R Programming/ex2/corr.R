# takes a directory of data files and a threshold for complete cases and 
# calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) is greater 
# than the threshold. The function should return a vector of correlations for 
# the monitors that meet the threshold requirement. If no monitors meet the 
# threshold requirement, then the function should return a numeric vector of 
# length 0.
corr <- function(directory, threshold = 0)
{    
    # get a data frame with the amount of complete observations in each monitor
    df = complete(directory)
    # create an empty numeric vector size of the amount of monitors that have
    # more complete observations than the threshold.
    corVec = numeric(sum(df[ ,"nobs"] > threshold))
    index = 1
    
    for (i in 1:nrow(df))
    {
        if (df[[i,"nobs"]] > threshold)
        {
            # create filename "directory/***.csv". *** is a number with 3 digits
            # (set leading zeros in case of less than 3 digits).
            fileName <- paste(directory, formatC(df[[i,"id"]], width=3, flag=0), 
                              sep = "/")
            fileName <- paste(fileName, "csv", sep = ".")
            
            # read data frame from file to data.
            data<-read.csv(fileName, header = TRUE, comment.char = "")
            
            # create logicals vectors that indicate where there are not NA 
            # values in sulfate and nitrate columns.
            sulfateNotNAValues <- !is.na(data[ ,"sulfate"])
            nitrateNotNAValues <- !is.na(data[ ,"nitrate"])
            
            # multiply logical vectors to get rows where both columns are not NA's
            # (AND condition).
            completeObservations = as.logical(
                                        sulfateNotNAValues*nitrateNotNAValues)
            
            corVec[index] <- cor(data[ ,"sulfate"][completeObservations], 
                                 data[ ,"nitrate"][completeObservations])
            index = index + 1
        }
    }
    
    corVec
}