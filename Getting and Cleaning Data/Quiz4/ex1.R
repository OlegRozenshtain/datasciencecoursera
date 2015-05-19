# The American Community Survey distributes downloadable data about United 
# States communities. Download the 2006 microdata survey about housing for the 
# state of Idaho using download.file() and load the data into R. The code book, 
# describing the variable names. 
# Apply strsplit() to split all the names of the data frame on the characters 
# "wgtp". What is the value of the 123 element of the resulting list?
ex1<-function()
{
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    download.file(fileUrl, destfile = "./Getting and Cleaning Data/Quiz4/ex1.csv", 
                  quiet = TRUE)
    data<-read.csv(file = "./Getting and Cleaning Data/Quiz4/ex1.csv")

    strsplit(x = names(data), split = "wgtp")[123]
}