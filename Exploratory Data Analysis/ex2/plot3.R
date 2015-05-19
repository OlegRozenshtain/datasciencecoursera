# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
# answer this question.
plot3<-function()
{
    # read Environmental Protection Agency database on emissions of PM2.5
    # for 1999, 2002, 2005, and 2008
    emissionsData<-readRDS("summarySCC_PM25.rds")
    
    # summarize the total pm2.5 emission for each year in Baltimore (fips=24510)
    # by type of source.
    emissionsSumBaltimore<-aggregate(Emissions ~ year + type,
                                     data = subset(emissionsData, fips == "24510"),
                                     sum)
    
    library(ggplot2)    # load ggplot2 package
    # save the plot to a png file
    png("plot3.png", width = 840)
    # create 4 series of total pm2.5 emission in tons for each year grouped by
    # the type of source
    p<-ggplot(emissionsSumBaltimore, 
           aes(x = year, y = Emissions, color = type, group = type)) 
    # add big points for the sum of pm2.5 emission values and connect them by a
    # line
    p<-p + geom_point(size = 4) + geom_line() 
    # add annotations
    p<-p + labs(title = "Total pm2.5 emission per year in Baltimore", 
                x = "Year", y = "pm2.5 emission [tons]")
    print(p)    # print the final plot
    dev.off()
}