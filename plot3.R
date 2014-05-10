# this function downloads archive and extract the file with power consumption data
loadAndUnpackData <- function(archiveUrl, filePath) { 
  temp <- tempfile();
  download.file(archiveUrl, temp, method = "curl");
  unzip(temp);
  unlink(temp)
}

filePath <- 'household_power_consumption.txt';
loadAndUnpackData('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip', filePath)

pwConsumption <- read.csv(
  filePath, 
  header = TRUE,
  sep = ";",
  na.strings = "?",
  colClasses <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
)

colnames(pwConsumption) <- c("date", 
                             "time",
                             "globalActivePower", 
                             "globalReactivePower",
                             "voltage",
                             "globalIntensity",
                             "subMetering1",
                             "subMetering2",
                             "subMetering3")

# we need only data for 1/2/2007 and 2/2/2007
pwConsumption <- subset(pwConsumption, date == "1/2/2007" | date == "2/2/2007" )

# adding datetime column
pwConsumption = cbind( pwConsumption, strptime(paste( pwConsumption$date, " ", pwConsumption$time), format="%d/%m/%Y %H:%M:%S"))
colnames(pwConsumption)[10] <- "datetime"

par(col = "black")
par(mfrow = c(1,1))

# setting english locale so that weekdays abbreviates will be shown correctly on the plot 
Sys.setlocale("LC_TIME", "C")

png(file = "plot3.png")

with(pwConsumption, 
     plot(datetime, subMetering1, type="l", ylab = "Energy sub metering", xlab = ""))
with(pwConsumption, 
     points(datetime, subMetering2, type="l", col = "red"))
with(pwConsumption, 
     points(datetime, subMetering3, type="l", col = "blue"))
legend("topright", col = c("black", "red", "blue"), 
       legend = c ("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1))

dev.off();