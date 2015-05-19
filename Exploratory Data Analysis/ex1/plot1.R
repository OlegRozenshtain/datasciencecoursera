plot1<-function()
{
    library(data.table)
    # raed all data as "character", and cast relevant variables later.
    data<-fread(input = "household_power_consumption.txt", sep = ";", 
             header = TRUE, na.strings = "?", colClasses = rep("character", 9))
    
    # cast into date format.
    data$Date<-as.Date(data$Date, format = "%d/%m/%Y")
    
    # filter out relevant observations. 
    data<- data[data$Date >= "2007/02/01" & data$Date <= "2007/02/02"]
    
    globalActivePower = as.numeric(data$Global_active_power)
    
    # build a histogram and and save it to png file.
    png("plot1.png", width = 480, height = 480)
    hist(globalActivePower, col = "red",
         xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
    dev.off()
}