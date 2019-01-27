

# title: "Coursera Exploratory Data Analysis Week 1 Project"
# author: "Connor Voglewede"
# date: "1/19/2019"
# output: R script

  
# Load required packages

library(tidyverse)
library(lubridate)

# download Zip file and extract .txt file

filename <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
t1 = tempdir()
tf = tempfile(tmpdir=t1, fileext=".zip")
download.file(filename, tf)
unzip(tf, exdir=t1, overwrite=TRUE)
epc <- read.table(file.path(t1, "household_power_consumption.txt"),sep=";",na.strings = "?",header=TRUE)

# Subset data to 2007-02-01 through 2017-02-02

epc$Date <- lubridate::dmy(epc$Date)
subsetDate <- interval("2007-02-01","2007-02-02")
epc_subset <- subset(epc,epc$Date %within% subsetDate)


# Plot 1


epc1 <- epc_subset
epc1$Global_active_power <- as.numeric(epc1$Global_active_power)
epc1$Global_active_power <- epc1$Global_active_power

hist(epc1$Global_active_power,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)",ylab="Frequency")


# Plot 2

epc2 <- epc_subset
epc2$Global_active_power <- as.numeric(epc2$Global_active_power)
epc2$DateTime <- lubridate::ymd_hms(paste(epc2$Date,epc2$Time))

plot(epc2$DateTime,epc2$Global_active_power,type="l",xlab=" ",ylab="Global Active Power (kilowatts)")

# Plot 3

epc3 <- epc_subset
epc3$DateTime <- lubridate::ymd_hms(paste(epc2$Date,epc2$Time))


plot(epc3$DateTime,epc3$Sub_metering_1,type="l",xlab=" ",ylab="Energy sub metering",col=1)
lines(epc3$DateTime,epc3$Sub_metering_2,type="l",col=2)
lines(epc3$DateTime,epc3$Sub_metering_3,type="l",col=4)
legend("topright",legend=c(names(epc3)[7:9]),col=c(1,2,4),lty=1)


# Plot 4

epc4 <- epc_subset
epc4$DateTime <- lubridate::ymd_hms(paste(epc2$Date,epc2$Time))

par(mfrow=c(2,2))

# upper left
epc4$Global_active_power <- as.numeric(epc4$Global_active_power)
plot(epc4$DateTime,epc4$Global_active_power,type="l",xlab=" ",ylab="Global Active Power (kilowatts)")

# upper right
plot(epc4$DateTime,epc4$Voltage,type="l",xlab="datetime",ylab="Voltage")


# bottom left
plot(epc4$DateTime,epc4$Sub_metering_1,type="l",xlab=" ",ylab="Energy sub metering",col=1)
lines(epc4$DateTime,epc4$Sub_metering_2,type="l",col=2)
lines(epc4$DateTime,epc4$Sub_metering_3,type="l",col=4)
legend("topright",legend=c(names(epc4)[7:9]),col=c(1,2,4),bty="n",lty=1)

# bottom right
plot(epc4$DateTime,epc4$Global_reactive_power,type="l",xlab="datetime",ylab=names(epc4$Global_reactive_power))

