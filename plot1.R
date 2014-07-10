## Plot1.R
## ---------------------
## Author: Mikkel Porse
## 
## File contents:
## This file defines two functions
## - readData1
## - createPlot1
## at the end, createPlot1 is called
##
## Usage: 
## source("Plot1.R") in R
##


## readData1
## -----------
## Assumes the presence of household_power_consumption.txt file in working directory
## containing the "Individual household electric power consumption Data Set".
##
## function only reads columns 1 and 3 of the dataset, as the rest aren't used
## Date-column is cast as Date.
readData1 <- function(){
  
  ## read data, skip time of day and other measurements
  filename <- "household_power_consumption.txt"
  data <- read.table(filename,sep=";", header=T, colClasses=c("character","NULL","numeric",rep("NULL", 6)),na.strings=c("?"))
  
  ## subset to days 1 and 2 of February 2007
  feb <- subset(data, Date=="1/2/2007" | Date=="2/2/2007",drop=F)
  
  ## cast Date-field to Date
  dateFormat <- "%d/%m/%Y"
  feb$Date <- as.Date(feb$Date, format=dateFormat)
  feb
}

## CreatePlot1
## -----------
## Produces one 480x480 pixel png named "plot1.png" in the working directory
## showing the distribution of measurements of Global Active Power consumption (in kilowatts)
## in the 48 hour time period of Feb. 1st 2007-Feb. 2nd 2007
createPlot1 <- function(){
  
  ## get data for plotting
  feb <- readData1()
  
  ## Set fontsize to match the example - sort of
  par(ps=13)
  
  ## produce histogram in png
  png("plot1.png", width=480, height=480, units="px")
  hist(feb$Global_active_power, breaks=12, col="red", main="Global Active Power",xlab="Global Active Power (kilowatts)")
  dev.off()
}

createPlot1()