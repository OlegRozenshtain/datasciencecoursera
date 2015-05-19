# Load the Gross Domestic Product data for the 190 ranked countries.
# Remove the commas from the GDP numbers in millions of dollars and average them. 
# What is the average?
ex2<-function()
{
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(fileUrl, quiet = TRUE,
                  destfile = "./Getting and Cleaning Data/Quiz4/GDP.csv")
    GDPData<-read.csv(file = "./Getting and Cleaning Data/Quiz4/GDP.csv", 
                      skip = 5, nrows = 190, header = FALSE, 
                      stringsAsFactors = FALSE)
    
    # remove commas from GDP numbers in millions of dollars (from inspecting the
    # table it is the fifth column)
    GDP<-gsub(",", "", GDPData$V5)
    
    # cast the amounts from character to numeric and average the result
    mean(as.numeric(GDP))
}