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
subsetConsumption <- subset(pwConsumption, date == "1/2/2007" | date == "2/2/2007" )

# adding datetime column
subsetConsumption = cbind( subsetConsumption, strptime(paste( subsetConsumption$date, " ", subsetConsumption$time), format="%d/%m/%Y %H:%M:%S"))
colnames(subsetConsumption)[10] <- "datetime"

par(col = "black")
par(mfrow = c(1,1))
with(subsetConsumption, 
     plot(datetime, globalActivePower, type="l", ylab = "Global Active Power (kilowatts)", xlab = ""))

dev.copy(png, file = 'plot2.png');
dev.off();