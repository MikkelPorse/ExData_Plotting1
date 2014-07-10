## Plot3.R
## ---------------------
## Author: Mikkel Porse
## 
## File contents:
## This file defines two function 
## - readData3
## - createPlot3
## at the end, createPlot3 is called
##
## Usage: 
## source("Plot3.R") in R
##

## readData3
## -----------
## Assumes the presence of household_power_consumption.txt file in working directory
## containing the "Individual household electric power consumption Data Set".
## 
## Columns 3-6 are omitted, since they aren't used for this plot
## A 6th column is created from Date and Time columns using strptime
readData3 <- function(){
  
  ## read data. Skips middle 4 columns as they aren't needed
  filename <- "household_power_consumption.txt"
  data <- read.table(filename,sep=";", header=T, colClasses=c("character","character",rep("NULL", 4),rep("numeric",3)),na.strings=c("?"))
  
  ## subset to first two days of February 2007
  feb <- subset(data, Date=="1/2/2007" | Date=="2/2/2007", drop=F)
  
  ## cast Date and Time-fields to Date-type
  dateTimeFormat <- "%d/%m/%Y %H:%M:%S"
  feb$combinedDate <- strptime(paste(feb$Date, feb$Time), format=dateTimeFormat)
  feb
}

## Produces one 480x480 pixel png named "plot3.png" in the working directory
## showing sub meterings 1 through 3 as a function of time in one plot, 
## using color codings to tell the data points apart
createPlot3 <- function(){
  
  feb <- readData3()
  
  ## For those of us using other languages than English
  ## Otherwise the Tick-marks on the x-axis would use 
  ## local daynames in stead of "Thu", "Fri" and "Sat"
  locale  <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "English") 
  
  ## Set fontsize to match the example - sort of
  par(ps=13)
  
  ## create the plot from empty
  png("plot3.png", width=480, height=480, units="px")
  
  ## Note that Sub_metering_1 maxes at 38, so use this 
  plot(feb$combinedDate, feb$Sub_metering_1, type="n",xlab="", ylab="Energy sub metering")
  lines(feb$combinedDate, feb$Sub_metering_1, col="black")
  lines(feb$combinedDate, feb$Sub_metering_2, col="red")
  lines(feb$combinedDate, feb$Sub_metering_3, col="blue")
  legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"),lwd=1)
  dev.off()
  
  # Reset locale
  Sys.setlocale("LC_TIME", substr(locale, 1, grep("_", strsplit(locale,"")[[1]])-1))
}

createPlot3()
