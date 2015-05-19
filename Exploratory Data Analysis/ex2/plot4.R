# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?
plot4<-function()
{
    # read Environmental Protection Agency database on emissions of PM2.5
    # for 1999, 2002, 2005, and 2008
    emissionsData<-readRDS("summarySCC_PM25.rds")
    
    #read mapping from the source classification code digit strings in the
    #Emissions table to the actual name of the PM2.5 source
    sourceClassificationCode<-readRDS("Source_Classification_Code.rds")
    
    # join emissions table with source classification code table by key column
    # 'SCC' (Source Classification Code)
    emissionsWithSourceClassification<-merge(emissionsData, 
                                             sourceClassificationCode,
                                             by.x = "SCC", by.y = "SCC")
    
    # filter the joined table to get only observations related to coal 
    # combustion sources
    coalCombustionEmissions<-subset(emissionsWithSourceClassification, 
                                    grepl("Coal", EI.Sector) &
                                    grepl("Comb", EI.Sector))
    
    # summarize the total pm2.5 emission from the filtered data for each year
    coalCombustionEmissionsSumByYear<-with(coalCombustionEmissions, 
                                           tapply(Emissions, year, sum))
    
    # save the plot to a png file
    png("plot4.png", width = 600)
    # create a bar showing the total pm2.5 emission in tons for each year
    barplot(coalCombustionEmissionsSumByYear, 
            main = "Total pm2.5 emission per year from coal combustion-related sources", 
            xlab = "Year", ylab = "pm2.5 emission [tons]")
    # add a line connecting the bars to show trend
    lines(coalCombustionEmissionsSumByYear, col = "green", lwd = 3)
    dev.off()
}