#3. PLOT3:

# We first we load the source file 

sourcefile <- "household_power_consumption.txt"
data <- read.table(sourcefile, header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ".")

# Subsetting data for mentioned dates
dateddata <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

# strptime used to Convert Characters to time objects

datetime <- strptime(paste(dateddata$Date, dateddata$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 

#Here numeric attempts to coerce its argument to numeric type and we select the features from the dataset to be plotted 

activepower <- as.numeric(dateddata$Global_active_power)
subMetering1 <- as.numeric(dateddata$Sub_metering_1)
subMetering2 <- as.numeric(dateddata$Sub_metering_2)
subMetering3 <- as.numeric(dateddata$Sub_metering_3)

# Plotting the final graph

png("plot3.png")
plot(datetime, subMetering1, type="l", ylab="Energy Submetering", xlab="")
lines(datetime, subMetering2, type="l", col="red")
lines(datetime, subMetering3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"), lty=1, lwd=2, )

