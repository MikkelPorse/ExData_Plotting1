## Plot2.R
## ---------------------
## Author: Mikkel Porse
## 
## File contents:
## This file defines two functions:
## - readData2
## - createPlot2
## at the end, createPlot2 is called
##
## Usage: 
## source("Plot2.R") in R

## readData2
## ---------
## Assumes the presence of household_power_consumption.txt file in working directory
## containing the "Individual household electric power consumption Data Set".
## Only columns 1-3 are needed, so the rest are skipped
## A 4th column is created from Date and Time columns using strptime
readData2 <- function(){
  ## read data. 
  filename <- "household_power_consumption.txt"
  data <- read.table(filename,sep=";", header=T, colClasses=c("character","character","numeric",rep("NULL", 6)),na.strings=c("?"))
  
  ## subset to first two days of February 2007
  feb <- subset(data, Date=="1/2/2007" | Date=="2/2/2007", drop=F)
  
  ## cast Date and Time-fields to Date-type
  dateTimeFormat <- "%d/%m/%Y %H:%M:%S"
  feb$combinedDate <- strptime(paste(feb$Date, feb$Time), format=dateTimeFormat)
  feb
}

## CreatePlot2
## -----------
## Produces one 480x480 pixel png named "plot2.png" in the working directory
## showing Global Active Power consumption in kilowatts as a function of time
createPlot2 <- function(){

  ## read data
  feb <- readData2()

  ## For those of us using other languages than English
  ## Otherwise the Tick-marks on the x-axis would use 
  ## local day names in stead of "Thu", "Fri" and "Sat"
  locale  <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "English") 
  
  ## Set fontsize to match the example - sort of
  par(ps=13)
  
  ## make the plot
  png("plot2.png",width=480, height=480, units="px")
  with(feb,plot(combinedDate, Global_active_power,type="l", xlab="", ylab="Global Active Power (kilowatts)"))
  dev.off()
  
  # Reset locale
  Sys.setlocale("LC_TIME", substr(locale, 1, grep("_", strsplit(locale,"")[[1]])-1))
}

createPlot2()