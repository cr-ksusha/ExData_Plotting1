# this function downloads archive and extract the file with power consumption data
loadAndUnpackData <- function(archiveUrl, filePath) { 
  temp <- tempfile();
  download.file(archiveUrl, temp, method = "curl");
  unzip(temp);
  unlink(temp)
}

filePath <- 'household_power_consumption.txt';
loadAndUnpackData('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip', filePath)

# this function finds the start row which we have to read and load the data from and 
# the number of rows we have to read
# this is necessary in order to avoid reading whole file in memory 
findBoundaries <- function( fileName ) {
  # create file descriptor
  con <- file(fileName);
  # open connection to a file
  open(con)
  lineNumber = 0
  startRow = NULL
  rowsToRead = 0
  # now read file line by line
  while(length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    lineNumber = lineNumber + 1
    if ( is.null(startRow) ) {
      # found needed date value
      if ( length(grep("^1/2/2007", line )) != 0 ) {
        startRow = lineNumber
        # this is the first line of the data we need to read
        rowsToRead = 1
      }
    } else { 
      # count the number of lines which contain needed dates
      if ( length(grep("^1/2/2007", line )) != 0 || length(grep("^2/2/2007", line )) != 0 ) {
        rowsToRead <- rowsToRead + 1
      } else {
        break;
      }
    }
  }
  close(con)
  vec <- c(startRow, rowsToRead)
  return(vec)
} 

# we have to read only rows that contains data for 2007-02-01 and 2007-02-02, 
# so we find row which we have to start to read from and the number of rows we have to read
# via findBoundaries function

boundaries <- findBoundaries(filePath)
startDataBoundary <- boundaries[1];
numberOfRows <- boundaries[2]

pwConsumption <- read.csv(
  filePath, 
  header = TRUE,
  sep = ";",
  na.strings = "?",
  skip  = startDataBoundary - 1,
  nrows = numberOfRows,
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

# constructing datetime from columns date and time and adding datetime column
pwConsumption = cbind( pwConsumption, strptime(paste( pwConsumption$date, " ", pwConsumption$time), format="%d/%m/%Y %H:%M:%S"))
colnames(pwConsumption)[10] <- "datetime"

par(col="black")

# setting an english locale so that weekdays abbreviates will be shown correctly on the plot 
Sys.setlocale("LC_TIME", "C")

png(file = "plot4.png")
# 2 rows and 2 columns for plots
par(mfrow = c(2,2));

# plot 1
with(pwConsumption, 
     plot(datetime, globalActivePower, type="l", ylab = "Global Active Power", xlab = ""))

# plot 2
with(pwConsumption, 
     plot(datetime, voltage, type="l", ylab = "Voltage"))

# plot 3
with(pwConsumption, 
     plot(datetime, subMetering1, type="l", ylab = "Energy sub metering", xlab = ""))
with(pwConsumption, 
     points(datetime, subMetering2, type="l", col = "red"))
with(pwConsumption, 
     points(datetime, subMetering3, type="l", col = "blue"))
legend("topright", col = c("black", "red", "blue"), 
       legend = c ("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), cex = 0.6)

# plot 4
with(pwConsumption, 
     plot(datetime, globalReactivePower, type="l", ylab = "Global_reactive_power"))

dev.off()
