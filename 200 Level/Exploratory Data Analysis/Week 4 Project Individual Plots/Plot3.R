
# title: "Coursera Exploratory Data Analysis Week 4 Project"
# author: "Connor Voglewede"
# date: "1/27/2019"
# output: Plot3.R, Plot3.png


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


# Question 3 

# Of the four types of sources indicated by the \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

# Answer
  # All of the types except "Point" have seen decreases to emissions levels in Baltimore City, MD from 1999 to 2008. However, the type "Point" has shown a dramatic decrease from 2005 to 2008, putting 2008 just slightly ahead of 1999 levels.

NEI %>% 
  subset(fips=="24510") %>% 
  group_by(year,type) %>% 
  summarise(TotalEmissions=sum(Emissions)) %>% 
  ggplot(aes(factor(year),y=TotalEmissions,fill=type))+geom_bar(stat="identity")+facet_wrap(.~type)+ggtitle("Baltimore City, MD Emissions by Emission Type")


dev.copy(png,'plot3.png')
dev.off()
