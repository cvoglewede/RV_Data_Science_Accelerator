
# title: "Coursera Exploratory Data Analysis Week 4 Project"
# author: "Connor Voglewede"
# date: "1/27/2019"
# output: Plot5.R, Plot5.png


# Load required packages

require(tidyverse)
require(lubridate)

# download Zip file and extract .txt file

filename <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
t1 = tempdir()
tf = tempfile(tmpdir=t1, fileext=".zip")
download.file(filename, tf)
unzip(tf, exdir=t1, overwrite=TRUE)

NEI <- readRDS(file.path(t1,"summarySCC_PM25.rds"))
SCC <- readRDS(file.path(t1,"Source_Classification_Code.rds"))


# Question 5

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Answer
  # Motor Vehicle emissions have fallen in Baltimore City, MD from 1999 to 2008.

SCC$MotorVehicle <- ifelse(grepl("Mobile - On-Road",SCC$EI.Sector,fixed=TRUE),"Motor Vehicle","Other")
SCC_Codes_Vehicle <- subset(SCC,MotorVehicle=="Motor Vehicle")[,"SCC"]


NEI %>% 
  subset(fips=="24510" & SCC %in% SCC_Codes_Vehicle) %>% 
  group_by(year) %>% 
  summarise(TotalEmissions=sum(Emissions)) %>% 
  ggplot(aes(factor(year),y=TotalEmissions))+geom_bar(stat="identity")+ggtitle("Motor Vehicle Emissions in Baltimore City, MD")


dev.copy(png,'plot5.png')
dev.off()
