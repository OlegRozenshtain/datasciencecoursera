# How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?
plot5<-function()
{
    # read Environmental Protection Agency database on emissions of PM2.5
    # for 1999, 2002, 2005, and 2008
    emissionsData<-readRDS("summarySCC_PM25.rds")
    
    #read mapping from the source classification code digit strings in the
    #Emissions table to the actual name of the PM2.5 source
    sourceClassificationCode<-readRDS("Source_Classification_Code.rds")
    
    # filter emissions data for only Baltimore City observations before joining
    # tables to improve performance (merge function takes a lot of time)
    emissionsDataBaltimore = subset(emissionsData, fips == "24510")
    
    # join emissions table with source classification code table by key column
    # 'SCC' (Source Classification Code)
    emissionsWithSourceClassification<-merge(emissionsDataBaltimore, 
                                             sourceClassificationCode,
                                             by.x = "SCC", by.y = "SCC")
    
    # filter the joined table to get only observations related to motor vehicles
    # sources 
    motorVehicleEmissionsBaltimore<-subset(emissionsWithSourceClassification, 
                                           Data.Category == "Onroad")
    
    # summarize the total pm2.5 emission from the filtered data for each year
    motorVehicleEmissionsSumByYear<-with(motorVehicleEmissionsBaltimore, 
                                         tapply(Emissions, year, sum))
    
    # save the plot to a png file
    png("plot5.png", width = 600)
    # create a bar showing the total pm2.5 emission in tons for each year
    barplot(motorVehicleEmissionsSumByYear, 
            main = "Total pm2.5 emission per year from motor vehicle-related sources", 
            xlab = "Year", ylab = "pm2.5 emission [tons]")
    # add a line connecting the bars to show trend
    lines(motorVehicleEmissionsSumByYear, col = "green", lwd = 3)
    dev.off()
}