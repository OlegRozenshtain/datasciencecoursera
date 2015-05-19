# Load the Gross Domestic Product data for the 190 ranked countries and load the
# educational data. Cut the GDP ranking into 5 separate quantile groups. Make a
# table versus Income.Group. How many countries are Lower middle income but
# among the 38 nations with highest GDP?
ex5<-function()
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
    
    library(Hmisc)
    # Cut the GDP ranking into 5 separate quantile groups, and add for each row
    # (country), its ranking quantile group into new column "rankQuantile"
    mergedData$rankQuantile = cut2(mergedData$GDP_Ranking, g = 5)
    table(mergedData$Income.Group, mergedData$rankQuantile)
}