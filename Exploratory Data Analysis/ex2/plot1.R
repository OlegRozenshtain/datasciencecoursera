# Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.
plot1<-function()
{
    # read Environmental Protection Agency database on emissions of PM2.5
    # for 1999, 2002, 2005, and 2008
    emissionsData<-readRDS("summarySCC_PM25.rds")
    
    # summarize the total pm2.5 emission for each year
    emissionsSumByYear<-with(emissionsData, tapply(Emissions, year, sum))
    
    # save the plot to a png file
    png("plot1.png", width = 600)
    # create a bar showing the total pm2.5 emission in tons for each year
    barplot(emissionsSumByYear, main = "Total pm2.5 emission per year", 
            xlab = "Year", ylab = "pm2.5 emission [tons]")
    # add a line connecting the bars to show trend
    lines(emissionsSumByYear, col = "green", lwd = 3)
    dev.off()
}