
library(tidyverse)
library(stringr)
library(lubridate)


frontierData <- read_csv("/Users/cvoglewede/Downloads/frontierNovData.csv")

searchEngInd <- read_csv("http://gs.statcounter.com/chart.php?device=Desktop%20%26%20Mobile%20%26%20Tablet%20%26%20Console&device_hidden=desktop%2Bmobile%2Btablet%2Bconsole&multi-device=true&statType_hidden=search_engine&region_hidden=ww&granularity=monthly&statType=Search%20Engine&region=Worldwide&fromInt=201711&toInt=201811&fromMonthYear=2018-11&toMonthYear=2018-11&csv=1")

str(frontierData)

frontierData %>% 
  filter(month(CallTs)==11 ) %>% 
  mutate(SearchEngineRollup=!is.na(SearchEngine)) %>% 
    group_by(SearchEngineRollup ) %>%
  summarize(VisitCount=n())

# frontierData %>% 
#   filter(month(CallTs)==11 & is.na(SearchEngine)) %>% 
#   group_by(SearchAdGroup ) %>%
#   # mutate(Xsell=iif)
#   summarize(VisitCount=n()) %>% 
#   arrange(desc(VisitCount))

searchEngRV <- frontierData %>% 
  filter(month(CallTs)==11 & !is.na(SearchEngine)) %>% 
  group_by(SearchEngine) %>%
  summarize(VisitCount=n()) %>% 
  mutate(VisitShare = VisitCount / sum(VisitCount))

searchEngRV
searchEngInd
searchEngInd_clean
searchEngAll

searchEngInd_clean <- searchEngInd %>% 
  mutate(VisitShare=`Market Share Perc. (Nov 2018)`/100) %>% 
  mutate(SearchEngine=`Search Engine`) 
searchEngRV[,1]=c("Google","bing","Yahoo!")

searchEngAll <- merge(searchEngRV,searchEngInd_clean,by="SearchEngine")
searchEngAll <- searchEngAll %>% 
  select(SearchEngine,VisitShare.x,VisitShare.y)

searchEngAll <- gather(searchEngAll,key=SE_Usage,value=Percent,2:3)
library(ggplot2)

ggplot(searchEngAll, aes(SearchEngine, Percent, fill=SE_Usage)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Search Engine Usage November 2018")
         