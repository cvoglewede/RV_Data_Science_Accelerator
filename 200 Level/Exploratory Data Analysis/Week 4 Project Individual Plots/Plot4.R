
# title: "Coursera Exploratory Data Analysis Week 4 Project"
# author: "Connor Voglewede"
# date: "1/27/2019"
# output: Plot4.R, Plot4.png



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


# Question 4

# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# Answer
  # US coal combustion emissions have fallen dramatically from 1999 levels.

SCC$SCC <- as.character(SCC$SCC)

SCC$CoalComb <- ifelse(SCC$EI.Sector=="Fuel Comb - Comm/Institutional - Coal","Coal Combusion","Other")
SCC_Codes_CoalComb <- subset(SCC,CoalComb=="Coal Combusion")[,"SCC"]


NEI %>%
  subset(SCC %in% SCC_Codes_CoalComb) %>% 
  group_by(year) %>%
  summarise(TotalEmissions=sum(Emissions)) %>% 
  ggplot(aes(factor(year),y=TotalEmissions,fill=year))+geom_bar(stat="identity")+ggtitle("United State Coal Combusion Emissions over time")

dev.copy(png,'plot4.png')
dev.off()
