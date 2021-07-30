#2. PLOT2:

# We first we load the source file 

file <- "household_power_consumption.txt"
data <- read.table(file, header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ".")

# Creating a subset of the data for the mentioned dates

subset <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

# Convert Chraracter to time object

datetime <- strptime(paste(subset$Date, subset$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 

##Selecting Global active power which we have to plot in plot1

activepower <- as.numeric(subset$Global_active_power)


# Plotting the data 

png("plot2.png")
plot(datetime, activepower, type="l", xlab="", ylab="Global Active Power (kilowatts)")

