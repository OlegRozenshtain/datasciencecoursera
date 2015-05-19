# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips ==
# "06037"). Which city has seen greater changes over time in motor vehicle
# emissions?
plot6<-function()
{
    # read Environmental Protection Agency database on emissions of PM2.5
    # for 1999, 2002, 2005, and 2008
    emissionsData<-readRDS("summarySCC_PM25.rds")
    
    #read mapping from the source classification code digit strings in the
    #Emissions table to the actual name of the PM2.5 source
    sourceClassificationCode<-readRDS("Source_Classification_Code.rds")
    
    # filter emissions data for only Baltimore City and Los Angeles County
    # observations before joining tables to improve performance (merge function
    # takes a lot of time)
    emissionsDataFiltered = subset(emissionsData, fips %in% c("24510", "06037"))
    
    # join emissions table with source classification code table by key column
    # 'SCC' (Source Classification Code)
    emissionsWithSourceClassification<-merge(emissionsDataFiltered, 
                                             sourceClassificationCode,
                                             by.x = "SCC", by.y = "SCC")
    
    # filter the joined table to get only observations related to motor vehicles
    # sources 
    motorVehicleEmissions<-subset(emissionsWithSourceClassification, 
                                  Data.Category == "Onroad")
    
    # summarize the total pm2.5 emission from the filtered data for each year
    # by city
    motorVehicleEmissionsSumByYear<-aggregate(Emissions ~ year + fips,
                                              data = motorVehicleEmissions, sum)
    # change levels from code to actual city name
    levels(motorVehicleEmissionsSumByYear$fips)<-c("Los Angeles", "Baltimore")
    
    # split the summarized emission by city, and caculate the differences 
    # between the years. transpose the result to get the cities in the rows
    # and the differences in the columns (2*3 matrix)
    differencesMotorVehicleEmissions<-t(with(motorVehicleEmissionsSumByYear, 
                                             sapply(split(Emissions, fips), diff)))
    
    # save the plot to a png file
    png("plot6.png", width = 720)
    # set general graphical parameters
    par(mfrow = c(1,2), mar = c(5.1,4.1,4.1,1.1), oma = c(0,0,3,0))
    with(motorVehicleEmissionsSumByYear, 
         {
            plot(year, Emissions, type = "n", xaxt = "n", 
                 main = "absolute values",
                 xlab = "Year", ylab = "pm2.5 emission [tons]")
            axis(side = 1, at = seq(1999, 2008, by = 3))
            points(year, Emissions, col = fips, pch = 19)
            legend("right", legend = levels(fips), pch = 19, 
                   col = c("cyan", "magenta"))
         })
    # connect the dots for Baltimore emissions by a line
    with(motorVehicleEmissionsSumByYear[
                                motorVehicleEmissionsSumByYear$fips == "24510",], 
         lines(year, Emissions, col = fips))
    # connect the dots for Los Angeles emissions by a line
    with(motorVehicleEmissionsSumByYear[
                                motorVehicleEmissionsSumByYear$fips == "06037",], 
         lines(year, Emissions, col = fips))

    # create a bar plot for changes in emissions over time. save the x 
    # coordinates of the center of the bars into xValues
    xValues<-barplot(differencesMotorVehicleEmissions, beside = TRUE,
                     col = c("cyan", "magenta"), ylim = c(-600, 400),
                     main = "changes from year to year", 
                     ylab = "pm2.5 difference [tons]", xlab = "Years")
    # add x axis with 3 values uniformly distributed between the location of the
    # first and the last bars
    axis(side = 1, at = seq(xValues[1], xValues[6], length.out = 3), 
         labels = c("1999-2002", "2002-2005", "2005-2008"))
    legend("topright", legend = levels(motorVehicleEmissionsSumByYear$fips),
           pch = 15, col = c("cyan", "magenta"))
    
    # add a title for both plots
    mtext("pm2.5 emission from motor vehicle-related sources", 
          line = 1, outer = TRUE, font = 2, cex = 1.5)
    dev.off()
}