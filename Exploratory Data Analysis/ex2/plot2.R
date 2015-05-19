# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a
# plot answering this question.
plot2<-function()
{
    # read Environmental Protection Agency database on emissions of PM2.5
    # for 1999, 2002, 2005, and 2008
    emissionsData<-readRDS("summarySCC_PM25.rds")
    
    # summarize the total pm2.5 emission for each year in Baltimore (fips=24510)
    emissionsSumByYearBaltimore<-with(subset(emissionsData, fips == "24510"), 
                                      tapply(Emissions, year, sum))
    
    # save the plot to a png file
    png("plot2.png", width = 600)
    # create a bar showing the total pm2.5 emission in tons for each year in
    # Baltimore
    barplot(emissionsSumByYearBaltimore, 
            main = "Total pm2.5 emission per year in Baltimore", 
            xlab = "Year", ylab = "pm2.5 emission [tons]")
    # add a line connecting the bars to show trend
    lines(emissionsSumByYearBaltimore, col = "green", lwd = 3)
    dev.off()
}