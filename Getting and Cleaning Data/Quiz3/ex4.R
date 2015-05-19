# Load the Gross Domestic Product data for the 190 ranked countries and load the
# educational data. Match the data based on the country shortcode. What is the
# average GDP ranking for the "High income: OECD" and "High income: nonOECD"
# group?
ex4<-function()
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
                              "./Getting and Cleaning Data/Quiz3/educational.csv")
    
    # Match the data based on the country shortcode
    mergedData<-merge(GDPData, educationalData, by.x = "CountryCode", 
                      by.y = "CountryCode")
    
    library(dplyr)
    # get average GDP ranking for each "Income.Group" level
    mergedData %>% group_by(Income.Group) %>% 
        summarize(rankAvg = mean(GDP_Ranking))
}