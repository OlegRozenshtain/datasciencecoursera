best <- function(state, outcome) 
{
    # Read outcome data
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
    
    # cast death rate columns from character to numeric and suppress warnings
    # regarding introducing NA values
    suppressWarnings(rawData[, 11]<-as.numeric(rawData[, 11]))
    suppressWarnings(rawData[, 17]<-as.numeric(rawData[, 17]))
    suppressWarnings(rawData[, 23]<-as.numeric(rawData[, 23]))
    
    # remove all lines with NA values in one of the death rate parameters
    data<-rawData[complete.cases(rawData[, c(11,17,23)]), ]
    
    # split the data frame by states and take only the data of the relevant 
    # state
    stateData<-split(data,data$State)[[state]]
    
    # find the minimal value of the relevant death rate outcome
    lowestDeathRate<-min(stateData[, index])
    
    # get all hospitals names with the minimal value of the relevant death rate
    # outcome
    bestCandidates<-stateData[stateData[, index] == lowestDeathRate, 2]
    
    # sort the names alphabeticaly and return the first one
    bestCandidates[order(bestCandidates)][1]
} 