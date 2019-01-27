
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
  ggplot(aes(factor(year),y=TotalEmissions,fill=type))+geom_bar(stat="identity")+facet_wrap(.~type)

 

 # Question 4
 
 # Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

SCC$SCC <- as.character(SCC$SCC)

SCC$CoalComb <- ifelse(SCC$EI.Sector=="Fuel Comb - Comm/Institutional - Coal","Coal Combusion","Other")
SCC_Codes_CoalComb <- subset(SCC,CoalComb=="Coal Combusion")[,"SCC"]


 NEI %>%
   subset(SCC %in% SCC_Codes_CoalComb) %>% 
   group_by(year) %>%
   summarise(TotalEmissions=sum(Emissions)) %>% 
   ggplot(aes(factor(year),y=TotalEmissions,fill=year))+geom_bar(stat="identity")
 
# Question 5

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
 
SCC$MotorVehicle <- ifelse(grepl("Mobile - On-Road",SCC$EI.Sector,fixed=TRUE),"Motor Vehicle","Other")
SCC_Codes_Vehicle <- subset(SCC,MotorVehicle=="Motor Vehicle")[,"SCC"]


 NEI %>% 
   subset(fips=="24510" & SCC %in% SCC_Codes_Vehicle) %>% 
   group_by(year) %>% 
   summarise(TotalEmissions=sum(Emissions)) %>% 
   ggplot(aes(factor(year),y=TotalEmissions))+geom_bar(stat="identity")
 
 
 # Question 6
 
 # Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (\color{red}{\verb|fips == "06037"|}fips=="06037"). Which city has seen greater changes over time in motor vehicle emissions?
 
 data <- NEI %>% 
   subset((fips=="24510"|fips=="06037") & SCC %in% SCC_Codes_Vehicle) %>% 
   mutate(city=ifelse(fips=="24510","Baltimore, MD","Los Angeles, CA")) %>% 
   group_by(year,city) %>%
   summarise(TotalEmissions=sum(Emissions)) %>% 
   spread(key=city,TotalEmissions)

data$`Baltimore,MD Delta` <- (data$`Baltimore, MD` - lag(data$`Baltimore, MD`))/lag(data$`Baltimore, MD`)
data$`Los Angeles, CA Delta` <- (data$`Los Angeles, CA` - lag(data$`Los Angeles, CA`))/lag(data$`Los Angeles, CA`)
data$`Baltimore,MD Delta`[1] <- 0
data$`Los Angeles, CA Delta`[1] <- 0


data %>% 
  select(year,`Baltimore,MD Delta`,`Los Angeles, CA Delta`) %>% 
  rename("Baltimore, MD"="Baltimore,MD Delta","Los Angeles, CA"="Los Angeles, CA Delta") %>% 
  gather(key="city",value="delta",-year) %>% 
  ggplot(aes(year,y=delta,color=city))+geom_line()
 
 