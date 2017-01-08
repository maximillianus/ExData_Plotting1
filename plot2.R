##plot2.R

plot2 <- function()
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
	png("plot2.png")
	plot(subsetdata$Datetime, subsetdata$Global_active_power, type="l", xlab="", ylab="Global Active Power(kilowatts)")
	dev.off()

}