# Download the Excel spreadsheet on Natural Gas Aquisition Program. Read rows
# 18-23 and columns 7-15 into R and assign the result to a variable called: dat.
# What is the value of: sum(dat$Zip*dat$Ext,na.rm=T).
ex3<-function()
{
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
    download.file(fileUrl, destfile = "./Getting and Cleaning Data/Quiz1/ex3.xlsx", 
                  quiet = TRUE, mode = "wb")
    dat<-read.xlsx(file = "./Getting and Cleaning Data/Quiz1/ex3.xlsx", 
                    sheetIndex = 1, colIndex = 7:15, rowIndex = 18:23)
    
    sum(dat$Zip*dat$Ext,na.rm=T)
}