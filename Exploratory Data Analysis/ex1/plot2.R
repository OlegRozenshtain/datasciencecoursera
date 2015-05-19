plot2<-function()
{
    library(data.table)
    library(lubridate)
    
    # raed all data as "character", and cast relevant variables later.
    data<-fread(input = "household_power_consumption.txt", sep = ";", 
                header = TRUE, na.strings = "?", colClasses = rep("character", 9))
    
    # cast into date format.
    data$Date<-as.Date(data$Date, format = "%d/%m/%Y")
    
    # filter out relevant observations.
    data<- data[data$Date >= "2007/02/01" & data$Date <= "2007/02/02"]
    
    globalActivePower = as.numeric(data$Global_active_power)
    # paste date and time into one variable.
    time<-ymd_hms(paste(data$Date, data$Time, sep = " "))
    
    # first create an empty graph scheme then add a line plot to it. save it all
    # to a png file.
    png("plot2.png", width = 480, height = 480)
    plot(time, globalActivePower, ylab = "Global Active Power (kilowatts)", 
         type = "n", xlab = "")
    lines(time, globalActivePower)
    dev.off()
}