rankhospital <- function(state, outcome, num = "best") 
{
    ## Read outcome data
    rawData<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check that state is valid
    if (!is.element(state, rawData$State))
    {
        stop("invalid state")
    }
    
    # Check that outcome is valid and assign corresponding column number
    if (outcome == "heart attack") index<-11
    else if (outcome == "heart failure") index<-17
    else if (outcome == "pneumonia") index<-23
    else stop("invalid outcome")
    
    # split the data frame by states and take only the data of the relevant 
    # state
    stateData<-split(rawData,rawData$State)[[state]]
    
    # cast death rate columns from character to numeric and suppress warnings
    # regarding introducing NA values
    suppressWarnings(stateData[, 11]<-as.numeric(stateData[, 11]))
    suppressWarnings(stateData[, 17]<-as.numeric(stateData[, 17]))
    suppressWarnings(stateData[, 23]<-as.numeric(stateData[, 23]))
    
    # remove all lines with NA values in one of the death rate parameters
    data<-stateData[complete.cases(stateData[, c(11,17,23)]), ]
    
    if (num == "best") num<-1
    else if (num == "worst") num<-nrow(data)
    
    # If num is larger than the number of hospitals in state, return NA.
    if (num > nrow(stateData)) return(NA)
    
    sortedData<-data[order(data[,index], data[,2]), ]
    sortedData[num,2]
}
