# Load the Gross Domestic Product data for the 190 ranked countries and load the
# educational data. Match the data based on the country shortcode. How many of
# the IDs match? Sort the data frame in descending order by GDP rank. What is 
# the 13th country in the resulting data frame?
ex3<-function()
{
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(fileUrl, quiet = TRUE,
                  destfile = "./Getting and Cleaning Data/Quiz3/GDP.csv")
    GDPData<-read.csv(file = "./Getting and Cleaning Data/Quiz3/GDP.csv", 
                      skip = 5, nrows = 190, header = FALSE, 
                      stringsAsFactors = FALSE)
    # use only 2 first relevat colomuns
    GDPData<-data.frame(CountryCode = GDPData[[1]], GDP_Ranking = GDPData[[2]])
    
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    download.file(fileUrl, quiet = TRUE, destfile = 
                      "./Getting and Cleaning Data/Quiz3/educational.csv")
    educationalData<-read.csv(file = 
                              "./Getting and Cleaning Data/Quiz3/educational.csv",
                              stringsAsFactors = FALSE)
    
    # Match the data based on the country shortcode
    mergedData<-merge(GDPData, educationalData, by.x = "CountryCode", 
                       by.y = "CountryCode")
    
    library(dplyr)
    # Sort the data frame in descending order by GDP rank
    sortedMergedData<-arrange(mergedData, desc(GDP_Ranking))
    
    list(IDs_match = nrow(mergedData), 
         country_13 = sortedMergedData$Long.Name[13])
}