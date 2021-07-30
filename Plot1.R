
#2. PLOT2:

# We first we load the data set

dataFile <- "household_power_consumption.txt"
data <- read.table(dataFile, header=TRUE, sep=";", stringsAsFactors=FALSE, dec=".")

# Subset of the data set with mentioned dates

subSetData <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

# Selecting the Global Active power feature to plot

globalActivePower <- as.numeric(subSetData$Global_active_power)

#Plotting the histogram

png("plot1.png")
hist(globalActivePower, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")


