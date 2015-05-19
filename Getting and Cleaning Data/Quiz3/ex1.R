# The American Community Survey distributes downloadable data about United 
# States communities. Download the 2006 microdata survey about housing for the 
# state of Idaho using download.file() and load the data into R. The code book, 
# describing the variable names. 
# Create a logical vector that identifies the households on greater than 10 
# acres who sold more than $10,000 worth of agriculture products. Assign that 
# logical vector to the variable agricultureLogical. Apply the which() function 
# like this to identify the rows of the data frame where the logical vector is 
# TRUE. which(agricultureLogical) What are the first 3 values that result?
ex1<-function()
{
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    download.file(fileUrl, destfile = "./Getting and Cleaning Data/Quiz3/ex1.csv", 
                  quiet = TRUE)
    data<-read.csv(file = "./Getting and Cleaning Data/Quiz3/ex1.csv")
    
    # get only the records where households on greater than 10 acres (variable 
    # ACR, category 3) who sold more than $10,000 worth of agriculture products
    #(variable AGS, category 6)
    agricultureLogical<-data$ACR == 3 & data$AGS == 6
    head(which(agricultureLogical), n = 3)
}