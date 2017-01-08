##plot4.R

plot4 <- function()
{
	##read data, create classes
	##this data reading assumes we and the files are in working 		directory. Read all as character to read in "?"
	##since the dataset is very big, I only read the first 80000 rows
	##which has included the required date range 
	rawelectricdata <- read.table("household_power_consumption.txt", sep=";", header=TRUE, colClasses=c(rep("character",9)), nrows=80000)
	
	##Cleaning the data
	##First select the rows that contain "?"
	nullrow <- which(rawelectricdata[,3] == "?")
	electricdata <- rawelectricdata[-nullrow,]
	
	##Create Datetime column to concatenate Date & Time
	electricdata$Datetime <- strptime(paste(electricdata$Date, electricdata$Time), "%d/%m/%Y %H:%M:%S")
	
	##Make date data into Date Class and the GlobalActivePower to numeric
	electricdata[,"Date"] <- as.Date(electricdata[,"Date"], "%d/%m/%Y")
	#electricdata[,3] <- as.numeric(electricdata[,3])
	for(i in 3:9)
	{
		electricdata[,i] <- as.numeric(electricdata[,i])
	}
	
	##subset data to only contain required date range
	subsetdata <- subset(electricdata, Date=="2007-02-01" | Date=="2007-02-02")
	
	
	
	##make plot and output it in png format
	png("plot4.png")
	
	##create area to draw plot
	par(mfrow=c(2,2))
	
	##Plot 1 Datetime vs Global Active Power
	plot(subsetdata$Datetime, subsetdata$Global_active_power, type="l", xlab="", ylab="Global Active Power")
	
	##Plot 2 Datetime vs Voltage
	plot(subsetdata$Datetime, subsetdata$Voltage, type="l", xlab="datetime", ylab="Voltage")
	
	##Plot 3 Datetime vs Energy Sub Metering
	plot(subsetdata$Datetime, subsetdata$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
	lines(subsetdata$Datetime, subsetdata$Sub_metering_2, col="red")
	lines(subsetdata$Datetime, subsetdata$Sub_metering_3, col="blue")
	legend("topright", c(names(electricdata)[7], names(electricdata)[8], names(electricdata)[9]), lty=c(1,1,1), col=c("black", "red", "blue"))
	
	##Plot 4 Datetime vs Global_reactive_power
	plot(subsetdata$Datetime, subsetdata$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
	
	dev.off()
	
}