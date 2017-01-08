##plot1.R

plot1 <- function()
{
	##read data, create classes
	##this data reading assumes we and the files are in working 		directory. Read all as character to read in "?" 
	rawelectricdata <- read.table("household_power_consumption.txt", sep=";", header=TRUE, colClasses=c(rep("character",9)))
	
	##Cleaning the data
	##First select the rows that contain "?"
	nullrow <- which(rawelectricdata[,3] == "?")
	electricdata <- rawelectricdata[-nullrow,]
	
	##Make date data into Date Class and the GlobalActivePower to numeric
	electricdata[,"Date"] <- as.Date(electricdata[,"Date"], "%d/%m/%Y")
	electricdata[,3] <- as.numeric(electricdata[,3])
	
	##subset data to only contain required date range
	subsetdata <- subset(electricdata, Date=="2007-02-01" | Date=="2007-02-02")
	
	##make plot and output it in png format
	png("plot1.png")
	hist(subsetdata[,3], col="red", xlab="Global Active Power(kilowatts)", ylab = "Frequency", main="Global Active Power")
	dev.off()

}