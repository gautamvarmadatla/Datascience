#4. PLOT4:

# We first we load the source file 

sourcefile <- "household_power_consumption.txt"
data <- read.table(sourcefile, header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ".")

# Subsetting data for mentioned dates in the project

date <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]
time <- strptime(paste(date$Date, date$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 

#Here numeric attempts to coerce its argument to numeric type and we select the features from the dataset to be plotted

activepower <- as.numeric(date$Global_active_power)
reactivepower <- as.numeric(date$Global_reactive_power)
voltage <- as.numeric(date$Voltage)
submeter1 <- as.numeric(date$Sub_metering_1)
submeter2 <- as.numeric(date$Sub_metering_2)
submeter3 <- as.numeric(date$Sub_metering_3)


# Plotting the graph


png("plot4.png")
par(mfrow = c(2, 2)) 

plot(time, activepower, type="l", xlab="", ylab="Global Active Power", cex=0.2)

plot(time, voltage, type="l", xlab="time", ylab="Voltage")

plot(time, submeter1, type="l", ylab="Energy submeter", xlab="")
lines(time, submeter2, type="l", col="red")
lines(time, submeter3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=, lwd=2.5, col=c("black", "red", "blue"), bty="o")

plot(time, reactivepower, type="l", xlab="time", ylab="Global_reactive_power")

dev.off()

