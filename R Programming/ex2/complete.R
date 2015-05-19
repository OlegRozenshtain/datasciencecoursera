# reads a directory full of files and reports the number of completely observed 
# cases in each data file. The function should return a data frame where the 
# first column is the name of the file and the second column is the number of 
# complete cases.
complete <- function(directory, id = 1:332)
{
    # create a data frame with two columns "id" and "nobs". the "id" column is
    # initilized with the input id vector. "nobs" column is initialized with 0's
    df = data.frame(id = id, nobs = numeric(length(id)))
    
    for (i in 1:length(id))
    {
        # create filename "directory/***.csv". *** is a number with 3 digits
        # (set leading zeros in case of less than 3 digits).
        fileName <- paste(directory, formatC(id[i], width=3, flag=0), sep = "/")
        fileName <- paste(fileName, "csv", sep = ".")
        
        # read data frame from file to data.
        data<-read.csv(fileName, header = TRUE, comment.char = "")
        
        # create logicals vectors that indicate where there are not NA values in
        # sulfate and nitrate columns.
        sulfateNotNAValues <- !is.na(data[ ,"sulfate"])
        nitrateNotNAValues <- !is.na(data[ ,"nitrate"])
        
        # multiply logical vectors to get rows where both columns are not NA's
        # (AND condition). sum the result vector to get the amount of complete
        # observations.
        completeObservations = sum(sulfateNotNAValues*nitrateNotNAValues)
        
        # assign the result to the data frame.
        df[[i,"nobs"]] <- completeObservations
    }
    
    df
}