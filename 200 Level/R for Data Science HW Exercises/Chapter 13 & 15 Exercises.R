
library("tidyverse")
library("lubridate")
library("stringr")
library(nycflights13)
library("maps")

# 13.4.6 #1-4
# 13.5.1 #1-6
# 15.3.1 #1-3
# 15.4.1 #1-3


# 13.4.6 1

delay_by_dest <- flights %>% 
  group_by(dest) %>% 
  summarise(avg_delay=mean(arr_delay,na.rm=TRUE))
  

delay_by_dest %>%
   left_join(airports, c("dest" = "faa")) %>%
  ggplot(aes(lon, lat,col=avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()


# 13.4.6 2

flights_location <- flights %>% 
  left_join(airports,by=c("origin"="faa")) %>% 
  rename(lat_origin=lat) %>% 
  rename(lon_origin=lon) %>% 
  left_join(airports,by=c("dest"="faa")) %>% 
  rename(lat_dest=lat) %>% 
  rename(lon_dest=lon) %>% 
  select(colnames(flights),lat_origin,lon_origin,lat_dest,lon_dest)  
  
flights_location

# 13.4.6 3

year <- 2018
flights %>% 
  left_join(planes,by="tailnum") %>% 
  rename(plane_year=year.y) %>% 
  mutate(plane_age=year-plane_year) %>% 
  group_by(plane_age) %>% 
  summarise(avg_delay=mean(arr_delay,na.rm=TRUE),
            flight_count=n()) %>% 
  ggplot(aes(x=plane_age,y=avg_delay)) + geom_point(aes(size=flight_count))

 
# 13.4.6 4

# Dew Point
flights %>% 
  left_join(weather, by=c("origin"="origin","year"="year","month"="month","day"="day","hour"="hour")) %>% 
  group_by(dewp) %>% 
  summarise(avg_delay=mean(arr_delay,na.rm = TRUE),flight_count=n()) %>% 
  ggplot(aes(x=dewp,y=avg_delay)) + geom_point(aes(color=flight_count))
  
# Humidity
flights %>% 
  left_join(weather, by=c("origin"="origin","year"="year","month"="month","day"="day","hour"="hour")) %>% 
  group_by(humid) %>% 
  summarise(avg_delay=mean(arr_delay,na.rm = TRUE),flight_count=n()) %>% 
  ggplot(aes(x=humid,y=avg_delay)) + geom_point(aes(color=flight_count))

# Wind speed
flights %>% 
  left_join(weather, by=c("origin"="origin","year"="year","month"="month","day"="day","hour"="hour")) %>% 
  group_by(wind_speed) %>% 
  summarise(avg_delay=mean(arr_delay,na.rm = TRUE),flight_count=n()) %>% 
  ggplot(aes(x=wind_speed,y=avg_delay)) + geom_point(aes(color=flight_count))

# Wind gust
flights %>% 
  left_join(weather, by=c("origin"="origin","year"="year","month"="month","day"="day","hour"="hour")) %>% 
  group_by(wind_gust) %>% 
  summarise(avg_delay=mean(arr_delay,na.rm = TRUE),flight_count=n()) %>% 
  ggplot(aes(x=wind_gust,y=avg_delay)) + geom_point(aes(color=flight_count))

# Precipitation
flights %>% 
  left_join(weather, by=c("origin"="origin","year"="year","month"="month","day"="day","hour"="hour")) %>% 
  group_by(precip) %>% 
  summarise(avg_delay=mean(arr_delay,na.rm = TRUE),flight_count=n()) %>% 
  ggplot(aes(x=precip,y=avg_delay)) + geom_point(aes(color=flight_count))

# Pressure
flights %>% 
  left_join(weather, by=c("origin"="origin","year"="year","month"="month","day"="day","hour"="hour")) %>% 
  group_by(pressure) %>% 
  summarise(avg_delay=mean(arr_delay,na.rm = TRUE),flight_count=n()) %>% 
  ggplot(aes(x=pressure,y=avg_delay)) + geom_point(aes(color=flight_count))

# Visibility
flights %>% 
  left_join(weather, by=c("origin"="origin","year"="year","month"="month","day"="day","hour"="hour")) %>% 
  group_by(visib) %>% 
  summarise(avg_delay=mean(arr_delay,na.rm = TRUE),flight_count=n()) %>% 
  ggplot(aes(x=visib,y=avg_delay)) + geom_point(aes(color=flight_count))


# Weather conditions such as high wind speed, high wind gusts, high rates of precipitation, low air pressure, and low visibility are more likely to result in arrival delays of aircraft.


# 13.5.1 1

flights %>% 
  group_by(carrier) %>% 
  summarise(count=n()) %>% 
  mutate(Share=count/sum(count)) %>% 
  arrange(desc(count))

flights %>% 
  filter(is.na(tailnum)) %>% 
  group_by(carrier) %>% 
  summarise(count=n()) %>% 
  mutate(Share=count/sum(count)) %>% 
  arrange(desc(count))

flights %>% 
  filter(!is.na(tailnum)) %>% 
  anti_join(planes, by = "tailnum") %>%
  group_by(carrier) %>% 
  summarise(count=n()) %>% 
  mutate(Share=count/sum(count)) %>% 
  arrange(desc(count))


airports[airports$faa %in% c("LGA","EWR","JFK"),]

airlines[airlines$carrier  %in% c("MQ","UA","US","9E","AA"),]

# There are two cases in which a plane's tailnumber from the flights dataset cannot be matched to the plans dataset. 
# In one case, the tailnumber is null in the flights dataset. 
# These instances are concentrated to 3 carriers: Endeavor Air, United, and US Airways.
# The other case is when a tailnumber is not null in flights but cannot be matched in the planes dataset.
# Here, two carriers, Envoy Air and American Airlines, are most likely to have tailnumbers that do not match up with the planes database.


# 13.5.1 2

flights %>% 
  group_by(tailnum) %>% 
  mutate(flight_count=n()) %>% 
  filter(flight_count>=100) 


# 13.5.1 3

library("fueleconomy")

vehicles %>% 
  semi_join(common,by=c("make","model"))


# 13.5.1 4

Bad48Hours <- flights %>% 
  group_by(year,month,day) %>% 
  summarise(total_delay=sum(arr_delay,na.rm = TRUE),total_flights=n()) %>% 
  mutate(Two_Day_Total_Delay=total_delay+lag(total_delay),Two_Day_Total_Flights=total_flights+lag(total_flights) ,Two_Day_Avg_Delay=Two_Day_Total_Delay/Two_Day_Total_Flights) %>% 
  arrange(desc(Two_Day_Avg_Delay))


High_Precip_weather_days <- weather %>% 
  group_by(year,month,day) %>% 
  summarise(value=mean(precip)) %>% 
  group_by(year) %>%
  mutate(mu=mean(value),sigma=sd(value)) %>% 
  mutate(zscore_precip=(value-mu)/sigma) %>% 
 # ungroup(year)
  arrange(desc(zscore_precip))

High_Wind_weather_days <- weather %>% 
  group_by(year,month,day) %>% 
  summarise(value=mean(wind_speed,na.rm=TRUE)) %>% 
  group_by(year) %>%
  mutate(mu=mean(value),sigma=sd(value)) %>% 
  mutate(zscore_wind=(value-mu)/sigma) %>% 
  # ungroup(year)
  arrange(desc(zscore_wind))
  

Bad48Hours %>% 
  left_join(High_Precip_weather_days,by=c("year"="year","month"="month","day"="day")) %>% 
  left_join(High_Wind_weather_days,by=c("year"="year","month"="month","day"="day")) %>% 
  select(year,month,day,Two_Day_Avg_Delay,zscore_precip,zscore_wind)


# In the interest of time, I looked at just precipitation and wind speed as the weather variables to compare
# the highest two-day delay periods. There does not appear to be any obvious pattern between the highest
# months of delay and precipitation. There may be a stronger relationship between two-day delay averages and 
# wind speeds, as those days were typically above average wind-speed days across the country. 



# 13.5.1 5

anti_join(flights, airports, by = c("dest" = "faa"))

# This join will return flights from the flights dataset for which the destination is not listed in the airports
# dataset. This could be because the destination is to a foreign country and some foreign airports are not listed
# in the dataset 'airports'


anti_join(airports, flights, by = c("faa" = "dest"))

# This join will return all airports which were not a destination for any flights in the flights dataset.



# 13.5.1 6

carrier_tailnum <- flights %>% 
  group_by(tailnum) %>% 
  summarise(carrier_count=n_distinct(carrier)) %>% 
  arrange(desc(carrier_count))

count(carrier_tailnum,carrier_count)
4026/(4026+17)
# = 0.99579

count(flights[flights$tailnum=="N232PQ",],carrier)

# 99.6% of valid tailnumbers only have 1 carrier over the course of 2013. 
# There seems to be ample evidence for a clear relationship between tailnumber and carrier.


# 15.3.1 1


library("forcats")

gss_cat %>% 
  ggplot(aes(rincome))+geom_bar()

# One problem with this plot is the way in which the x labels overlap with each other and make the plot
# difficult to read and interpret.

gss_cat %>% 
  ggplot(aes(rincome))+geom_bar()+theme(axis.text.x = element_text(angle=90))

# The adjustment above, utilizing theme and axis.text.x allows us to rotate the x labels to 90 degree angle.


# 15.3.1 2

gss_cat %>% 
  count(relig,sort = TRUE)

# Protestant is the most common religion in the gss_cat dataset.

gss_cat %>% 
  count(partyid,sort = TRUE)

# Independent is the most common party id in the gss_cat dataset.


# 15.3.1 3

gss_cat %>% 
  filter(denom!="Not applicable") %>% 
  count(relig,denom,sort=TRUE)

# It appears that denomination applies specifically to the 'Protestant' value of religion.
# Visually we can find this answer as shown below:

gss_cat %>% 
  count(relig,denom) %>% 
  ggplot(aes(x=relig,y=denom))+geom_point(aes(size=n))



# 15.4.1 1

gss_cat %>% 
  ggplot(aes(y=tvhours))+geom_boxplot()+coord_flip()

gss_cat %>% 
  ggplot(aes(tvhours))+geom_bar()


# There are large number of outliers in the variable tvhours. Using median instead of mean may be more appropriate.


# 15.4.1 2

str(gss_cat)

keep(gss_cat, is.factor) %>% names()

levels(gss_cat[["marital"]])
levels(gss_cat[["race"]])
levels(gss_cat[["rincome"]])
levels(gss_cat[["partyid"]])
levels(gss_cat[["relig"]])
levels(gss_cat[["denom"]])

# The following variables are factors: marital, race, rincome, partyid, relig, and denom.
# marital: generally principled (never -> married, with some arbitrary factors in between)
# race: arbitrary
# rincome: principled
# partyid: principled
# relig: arbitrary
# denom: arbitrary

# 15.4.1 3

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(age, rincome)) + geom_point()
ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()
ggplot(rincome_summary, aes(age, fct_relevel("Not applicable",rincome))) +geom_point()

gss_cat$rincome[gss_cat$rincome=="Not applicable"]
as.numeric(gss_cat$rincome[gss_cat$rincome=="Not applicable"])
as.numeric(gss_cat$rincome[gss_cat$rincome=="No answer"])
as.numeric(gss_cat$rincome[gss_cat$rincome=="Refused"])
as.numeric(gss_cat$rincome[gss_cat$rincome=="Lt $1000"])
max(as.numeric(gss_cat$rincome[gss_cat$rincome !="Not applicable"]))

# My assumption for how R handle factor levels is that when we don't specify a factor relevel, 
# ggplot will order the factor by their level value. "Not applicable" is the highest level at the outset, 
# so it appears first. After using fct_relevel to send "Not applicable behind the rest of rincome
# factors, it becomes the last factor shown. "Lt $1000" becomes the highest level. 






