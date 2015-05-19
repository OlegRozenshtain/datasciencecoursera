# In the data set from Question 2 what is a regular expression that would allow
# you to count the number of countries whose name begins with "United"?
# How many countries begin with United?
ex3<-function()
{
    GDPData<-read.csv(file = "./Getting and Cleaning Data/Quiz4/GDP.csv", 
                      skip = 5, nrows = 190, header = FALSE, 
                      stringsAsFactors = FALSE)
    
    # from inspecting the table it is the fourth column is the countries names
    length(grep("^United",GDPData$V4))
}