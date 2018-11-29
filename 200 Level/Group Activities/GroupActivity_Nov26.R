library(tidyverse)
library(stringr)
library(lubridate)

frontierData <- read_csv("/Users/cvoglewede/Downloads/frontierNovData.csv")


head(frontierData)


frontierData %>%
  summarise(calls=sum(IsNetIBS,na.rm = TRUE),
            orders=sum(OrderCount,na.rm = TRUE),
            avg_RR=sum(orders)/sum(calls)
            )



frontierData %>%
  mutate(CallDate=substr(WebsessionDateDK,0,8)) %>%
  group_by(CallDate) %>%
  summarise(calls=sum(IsNetIBS,na.rm = TRUE),
            orders=sum(OrderCount,na.rm = TRUE),
            avg_RR=sum(orders)/sum(calls)) %>%
  arrange(CallDate)
  

frontierData %>%
  mutate(CallDate=as.Date(CallTs)) %>%
  group_by(CallDate) %>%
  summarise(calls=sum(IsNetIBS,na.rm = TRUE),
            orders=sum(OrderCount,na.rm = TRUE),
            avg_RR=sum(orders)/sum(calls)) %>%
  arrange(CallDate)
  

RR_by_Date <- frontierData %>%
  mutate(CallDate=as.Date(CallTs)) %>%
  group_by(CallDate) %>%
  summarise(calls=sum(IsNetIBS,na.rm = TRUE),
            orders=sum(OrderCount,na.rm = TRUE),
            avg_RR=sum(orders)/sum(calls)) %>%
  arrange(CallDate)


ggplot(RR_by_Date,aes(x=CallDate,y=avg_RR))+geom_line()

frontierData %>%
  group_by(SearchEngine) %>%
  summarise(Revenue=sum(EstimatedRevenue,na.rm = TRUE),
            Orders=sum(OrderCount,na.rm = TRUE),
    Rev_per_Order=sum(EstimatedRevenue,na.rm = TRUE)/sum(OrderCount,na.rm = TRUE))


frontierData %>%
  mutate(CallHour=hour(as_datetime(CallTs))) %>%
  group_by(SearchEngine,CallHour) %>%
  summarise(Revenue=sum(EstimatedRevenue,na.rm = TRUE),
            Orders=sum(OrderCount,na.rm = TRUE),
            Rev_per_Order=sum(EstimatedRevenue,na.rm = TRUE)/sum(OrderCount,na.rm = TRUE))


Rev_Grouped <- frontierData %>%
  mutate(CallHour=hour(as_datetime(CallTs))) %>%
  group_by(SearchEngine,CallHour) %>%
  summarise(Revenue=sum(EstimatedRevenue,na.rm = TRUE),
            Orders=sum(OrderCount,na.rm = TRUE),
            Rev_per_Order=sum(EstimatedRevenue,na.rm = TRUE)/sum(OrderCount,na.rm = TRUE))


ggplot(Rev_Grouped,aes(x=CallHour,y=Rev_per_Order,col=SearchEngine))+geom_line()
  
  
frontierData %>%
    # mutate(CallHour=hour(as_datetime(CallTs))) %>%
  filter(ServiceCheckInd==1) %>%
    group_by(State) %>%
    summarise(Serviceable=sum(ServiceableInd,na.rm = TRUE),
              Checks=sum(ServiceCheckInd,na.rm = TRUE),
              Serv_Rate=sum(ServiceableInd,na.rm = TRUE)/sum(ServiceCheckInd,na.rm = TRUE)) %>%
  arrange(desc(Serviceable))



# Assuming an order was placed and a product was sold, 
# how does talk time vary with the number of products sold 
# on a call on average in CT for users on Chrome Mobile and Safari browsers?

frontierData %>%
  filter(OrderCount==1,State=="CT",Browser in ("Chrome"))


head(RR_by_Date)

str(frontierData)

summary(frontierData$State)
View(frontierData)


