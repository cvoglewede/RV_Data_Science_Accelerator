
# title: "Coursera Exploratory Data Analysis Week 4 Project"
# author: "Connor Voglewede"
# date: "1/25/2019"
# output: R script


# Load required packages

library(tidyverse)
library(lubridate)


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

question1 <- NEI %>% 
  group_by(year) %>% 
  summarise(TotalEmissions=sum(Emissions)) 
 
par(mfrow=c(1,1))
options(scipen=999)
barplot(question1$TotalEmissions,names.arg=question1$year,xlab="Year",ylab="PM2.5 Emissions (tons)",main="Total PM2.5 Emissions by Year")


# Question 2 

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (\color{red}{\verb|fips == "24510"|}fips=="24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

question2 <- NEI %>% 
  subset(fips=="24510") %>% 
  group_by(year) %>% 
  summarise(TotalEmissions=sum(Emissions)) 

barplot(question2$TotalEmissions,names.arg=question2$year,xlab="Year",ylab="PM2.5 Emissions (tons)",main="Total PM2.5 Emissions by Year in Baltimore City, MD")


# Question 3 

# Of the four types of sources indicated by the \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


 NEI %>% 
  subset(fips=="24510") %>% 
  group_by(year,type) %>% 
  summarise(TotalEmissions=sum(Emissions)) %>% 
  ggplot(aes(x=year,y=TotalEmissions,col=type))+geom_line()


 # Question 4
 
 # Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

SCC$SCC <- as.character(SCC$SCC)
SCC$Short.Name <- as.character(SCC$Short.Name)

 # NEI %>% 
 #   merge(SCC,by.x="SCC",by.y="SCC",all=TRUE) %>% 
 #   group_by(Short.Name) %>% 
 #   summarise(TotalEmissions=sum(NEI$Emissions))
 # 
 #    is.recursive(SCC$Short.Name)
 
#   head(SCC)
# head(SCC$SCC)  
# levels(SCC$Option.Group)
