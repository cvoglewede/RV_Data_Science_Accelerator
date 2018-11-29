

# dir.create("Coursera_Getting_and_Cleaning_Data")

setwd("Coursera_Getting_and_Cleaning_Data")
getwd()

library(swirl)
library(data.table)
library(tidyr)
library(XML)
library(readxl)
library(XLConnect)
library(rJava)
library(xlsx)
library(microbenchmark)

df1 <- read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")


length(which(df1$VAL==24))

head(df1[,"FES"],100)


dat <- read_excel("/Users/cvoglewede/Downloads/Book1.xlsx")

sum(dat$Zip*dat$Ext,na.rm=T)


fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileURL)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)

xmlSApply(rootNode,xmlValue)
zipcodes <- xpathSApply(doc,"//[@class='zipcode']",xmlValue)


DT <- fread("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv")
head(DT)
mean(DT$pwgtp15)

tapply(DT$pwgtp15,DT$SEX,mean)
DT[,mean(pwgtp15),by=SEX]
mean(DT$pwgtp15,by=DT$SEX)
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
sapply(split(DT$pwgtp15,DT$SEX),mean)
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]

microbenchmark(mean(DT$pwgtp15,by=DT$SEX),DT[,mean(pwgtp15),by=SEX],times=100)

