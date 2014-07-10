## Plot4.R
## ---------------------
## Author: Mikkel Porse
## 
## File contents:
## This file defines two functions
## - readData4
## - createPlot4
## at the end, createPlot4 is called
##
## Usage:
## source("Plot4.R") in R
##


## readData4
## ----------
## Assumes the presence of household_power_consumption.txt file in working directory
## containing the "Individual household electric power consumption Data Set".
## A 10th column is created from Date and Time columns using strptime
readData4 <- function(){
  ## read data
  filename <- "household_power_consumption.txt"
  data <- read.table(filename,sep=";", header=T, colClasses=c("character","character",rep("numeric", 7)),na.strings=c("?"))
  
  ## subset to first two days of February 2007
  feb <- subset(data, Date=="1/2/2007" | Date=="2/2/2007", drop=F)
  
  ## cast Date and Time-fields to Date-type
  dateTimeFormat <- "%d/%m/%Y %H:%M:%S"
  feb$combinedDate <- strptime(paste(feb$Date, feb$Time), format=dateTimeFormat)  
  feb
}


## CreatePlot4
## -----------
## Produces one 480x480 pixel png named "plot4.png" in the working directory
## with 4 graphs in a 2x2 grid:
## Top left: Line plot of Global Active Power consumption by time
## Top right: Line plot of Voltage by time
## Bottom left: The three energy sub metering measures by time
## Bottom right: Global Reactive Power by time
createPlot4 <- function(){
  
  feb <- readData4()  
  ## For those of us using other languages than English
  ## Otherwise the Tick-marks on the x-axis would use 
  ## local daynames in stead of "Thu", "Fri" and "Sat"
  locale  <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "English") 
  
  ## Set fontsize to match the example - sort of
  par(ps=13)
  
  
  png("plot4.png", width=480, height=480, units="px")
  ## setup a 2x2 plot
  par(mfrow=c(2,2))

  ## upper left
  with(feb,plot(combinedDate, Global_active_power,type="l",xlab="", ylab="Global Active Power"))
  
  
  ## upper right
  with(feb, plot(combinedDate, Voltage, type="l", xlab="datetime"))

  
  ## lower left
  plot(feb$combinedDate, feb$Sub_metering_1, type="n",xlab="", ylab="Energy sub metering")
  ## make sure there's room for the legend
  legend("topright",c("Sub_metering_1","Sub_metering_1","Sub_metering_1"),
         col=c("black","red", "blue"),lwd=1,bty="n", plot = FALSE)  
  lines(feb$combinedDate, feb$Sub_metering_1, col="black")
  lines(feb$combinedDate, feb$Sub_metering_2, col="red")
  lines(feb$combinedDate, feb$Sub_metering_3, col="blue")
  legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         col=c("black", "red", "blue"),lwd=1,bty="n")
  
  
  ## lower right
  with(feb, plot(combinedDate,Global_reactive_power,type="l",lwd=0.1, xlab="datetime"))
  dev.off();
  
  # Reset locale
  Sys.setlocale("LC_TIME", substr(locale, 1, grep("_", strsplit(locale,"")[[1]])-1))
}

createPlot4()