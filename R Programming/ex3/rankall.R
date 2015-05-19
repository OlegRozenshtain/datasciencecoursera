rankall <- function(outcome, num = "best") 
{
    ## Read outcome data
    rawData<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check that outcome is valid and assign corresponding column number
    if (outcome == "heart attack") index<-11
    else if (outcome == "heart failure") index<-17
    else if (outcome == "pneumonia") index<-23
    else stop("invalid outcome")
    
    # cast death rate relevant column from character to numeric and suppress
    # warnings regarding introducing NA values
    suppressWarnings(rawData[, index]<-as.numeric(rawData[, index]))
    # remove all lines with NA values in relevant death rate parameter
    data<-rawData[complete.cases(rawData[, index]), ]
    
    # split the data by states. for each state sort the hospitals first by
    # relevant death rate parameter, then alphabetically by name
    dataByState<-split(data, data$State)
    dataByStateSorted<-lapply(dataByState, function(df, ind=index) 
                                           df[order(df[,ind], df[,2]), ])
    
    # create empty results data frame
    result<-data.frame(hospital=character(length(dataByStateSorted)),
                       state=character(length(dataByStateSorted)), 
                       stringsAsFactors = FALSE)
    
    # loop over the states
    for (i in 1:length(dataByStateSorted))
    {
        # assign state name 
        result$state[i]<-dataByStateSorted[[i]][["State"]][1]
        
        if (num == "best") rowNum<-1
        else if (num == "worst") rowNum<-nrow(dataByStateSorted[[i]])
        else rowNum<-num
        
        if (rowNum > nrow(dataByStateSorted[[i]]))
        {
            result$hospital[i]<-NA
        }
        else
        {
            result$hospital[i]<-dataByStateSorted[[i]][["Hospital.Name"]][rowNum]
        }
    }
    
    result
}