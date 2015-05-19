# Load the Gross Domestic Product data for the 190 ranked countries and load the
# educational data. Match the data based on the country shortcode.
# Of the countries for which the end of the fiscal year is available, how many
# end in June?
ex4<-function()
{
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(fileUrl, quiet = TRUE,
                  destfile = "./Getting and Cleaning Data/Quiz4/GDP.csv")
    GDPData<-read.csv(file = "./Getting and Cleaning Data/Quiz4/GDP.csv", 
                      skip = 5, nrows = 190, header = FALSE, 
                      stringsAsFactors = FALSE)
    # use only 2 first relevat colomuns
    GDPData<-data.frame(CountryCode = GDPData[[1]], GDP_Ranking = GDPData[[2]])
    
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    download.file(fileUrl, quiet = TRUE, destfile = 
                      "./Getting and Cleaning Data/Quiz4/educational.csv")
    educationalData<-read.csv(file = 
                                  "./Getting and Cleaning Data/Quiz4/educational.csv",
                              stringsAsFactors = FALSE)
    
    # Match the data based on the country shortcode
    mergedData<-merge(GDPData, educationalData, by.x = "CountryCode", 
                      by.y = "CountryCode")
    
    # from inspecting the education table we can realize that end of the fiscal
    # year is mentioned in the Special.Notes variable. The format is:
    # "Fiscal year end: 'month name' 'day as number'; 'other comments' "
    
    # get all the cases the fiscal year is available.
    endFiscalYearAvailable<-grep(pattern = "Fiscal year end:", 
                                 x = mergedData$Special.Notes, value = TRUE)
    
    # split the strings we got by "Fiscal year end: ", that way the first elemnt
    # is all the notes that comes prior to the date the fiscal year ends
    # (irrelevant information), the second element starts with the month we are
    # looking for.
    splitEndFiscalYearAvailable<-strsplit(endFiscalYearAvailable, 
                                          "Fiscal year end: ")
    
    # assemble a list of only the relevant parts (the second element of each 
    # sub-list)
    relevantPartOfNote<-sapply(splitEndFiscalYearAvailable, function(x) x[2])
    
    # count the countries in which the end of the fiscal year is in June
    length(grep(pattern = "^[Jj]une", x = relevantPartOfNote))
}