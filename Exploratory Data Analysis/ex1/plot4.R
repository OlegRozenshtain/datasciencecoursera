plot4<-function()
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
    globalReactivePower = as.numeric(data$Global_reactive_power)
    voltage = as.numeric(data$Voltage)
    subMetering1<-as.numeric(data$Sub_metering_1)
    subMetering2<-as.numeric(data$Sub_metering_2)
    subMetering3<-as.numeric(data$Sub_metering_3)
    # paste date and time into one variable.
    time<-ymd_hms(paste(data$Date, data$Time, sep = " "))
    
    # create 4 plots in 2*2 representation. save it all to a png file.
    png("plot4.png", width = 480, height = 480)
    par(mfrow = c(2,2))
    
    plot(time, globalActivePower, ylab = "Global Active Power", type = "n", 
         xlab = "")
    lines(time, globalActivePower)
    
    plot(time, voltage, ylab = "Voltage", xlab = "datetime", type = "n")
    lines(time, voltage)
    
    plot(time, subMetering1, ylab = "Energy sub metering", type = "n", xlab = "")
    lines(time, subMetering1, col = "black")
    lines(time, subMetering2, col = "red")
    lines(time, subMetering3, col = "blue")
    legend("topright", pch = "-", col = c("black", "red", "blue"), bty = "n",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    plot(time, globalReactivePower, ylab = "Global_reactive_power", 
         xlab = "datetime", type = "n")
    lines(time, globalReactivePower)
    dev.off()
}