# The American Community Survey distributes downloadable data about United 
# States communities. Download the 2006 microdata survey about housing for the 
# state of Idaho using download.file() and load the data into R. The code book,
# describes the variables names.
# How many properties are worth $1,000,000 or more?
ex1<-function()
{
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    download.file(fileUrl, destfile = "./Getting and Cleaning Data/Quiz1/ex1.csv", 
                  quiet = TRUE)
    data<-read.csv(file = "./Getting and Cleaning Data/Quiz1/ex1.csv")
    
    # get only the records where property value (VAL) is worth $1,000,000 or 
    # more (option 24).
    tmp<-data$VAL[data$VAL==24]
    rslt<-tmp[complete.cases(tmp)]  # remove all NA values
    length(rslt)
}