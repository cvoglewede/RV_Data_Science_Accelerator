
# title: "Coursera Exploratory Data Analysis Week 4 Project"
# author: "Connor Voglewede"
# date: "1/27/2019"
# output: Plot1.R, Plot1.png


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

# Question 1

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# Answer: 
  # Yes, Total PM2.5 Emissions have decreased from 1999 to 2008.

question1 <- NEI %>% 
  group_by(year) %>% 
  summarise(TotalEmissions=sum(Emissions)) 

par(mfrow=c(1,1))
options(scipen=999)
barplot(question1$TotalEmissions,names.arg=question1$year,xlab="Year",ylab="PM2.5 Emissions (tons)",main="Total PM2.5 Emissions by Year")

dev.copy(png,'plot1.png')
dev.off()
