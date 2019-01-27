
# title: "Coursera Exploratory Data Analysis Week 4 Project"
# author: "Connor Voglewede"
# date: "1/27/2019"
# output: Plot6.R, Plot6.png


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


# Question 6

# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (\color{red}{\verb|fips == "06037"|}fips=="06037"). Which city has seen greater changes over time in motor vehicle emissions?

# Answer
  # Baltimore City has consistently seen larger YoY percentage declines than Los Angeles.

SCC$MotorVehicle <- ifelse(grepl("Mobile - On-Road",SCC$EI.Sector,fixed=TRUE),"Motor Vehicle","Other")
SCC_Codes_Vehicle <- subset(SCC,MotorVehicle=="Motor Vehicle")[,"SCC"]

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
  ggplot(aes(year,y=delta,color=city))+geom_line()+ggtitle("YoY Changes in Motor Vehicle Emissions")


dev.copy(png,'plot6.png')
dev.off()
