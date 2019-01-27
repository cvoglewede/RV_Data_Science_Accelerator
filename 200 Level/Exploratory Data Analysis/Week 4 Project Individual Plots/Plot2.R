
# title: "Coursera Exploratory Data Analysis Week 4 Project"
# author: "Connor Voglewede"
# date: "1/27/2019"
# output: Plot2.R, Plot2.png


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

# Question 2 

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (\color{red}{\verb|fips == "24510"|}fips=="24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# Answer:
  # Yes, Total PM2.5 emissions have fallen in Baltimore City, MD from 1999 to 2008, but emissions increased in the period between 2002 and 2005.

question2 <- NEI %>% 
  subset(fips=="24510") %>% 
  group_by(year) %>% 
  summarise(TotalEmissions=sum(Emissions)) 

barplot(question2$TotalEmissions,names.arg=question2$year,xlab="Year",ylab="PM2.5 Emissions (tons)",main="Total PM2.5 Emissions by Year in Baltimore City, MD")

dev.copy(png,'plot2.png')
dev.off()
