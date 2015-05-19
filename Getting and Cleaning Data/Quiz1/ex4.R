# Read the XML data on Baltimore restaurants.
# How many restaurants have zipcode 21231?
ex4<-function()
{
    fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
    download.file(fileUrl, destfile = "./Getting and Cleaning Data/Quiz1/ex4.xml",
                  quiet = TRUE)
    data<-xmlTreeParse("./Getting and Cleaning Data/Quiz1/ex4.xml", 
                       useInternalNodes = TRUE)
    
    zipcodes<-xpathSApply(data, "//zipcode", xmlValue)
    
    length(zipcodes[zipcodes == 21231])
}