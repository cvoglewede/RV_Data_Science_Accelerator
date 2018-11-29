## Connor Voglewede
## Week 3 R Homework
## 5.5.2 1-5 , 
## 5.6.7 2-6 ,
## 5.7.1 1-8 ,
## 19.3.1 1-4 ,
## 19.4.4 1-6


## 5.5.2 1:


library(nycflights13)
library(tidyverse)

mutate(flights,
dep_time_minutes = (60 * dep_time %/% 100 + dep_time %% 100) %% 1440,
  sched_dep_time_minutes = (60 * sched_dep_time %/% 100 + sched_dep_time %% 100) %% 1440
)

transmute(flights,
          dep_time,
          sched_dep_time,
       dep_time_minutes = (60 * dep_time %/% 100 + dep_time %% 100) %% 1440,
       sched_dep_time_minutes = (60 * sched_dep_time %/% 100 + sched_dep_time %% 100) %% 1440
)

## 5.5.2 2:

# I expect to see an incorrect calculation in the new field, where the output is the difference of the integer 
# values of arr_time and dep_time. This is exactly what the calculation below shows. The problem is that 
# arr_time and dep_time are dates in the format HHMM, not integers. The second transmute function should
# this error. I would expect air_time_minutes to match air_time.

transmute(flights,
          arr_time,
          dep_time,
          air_time,
          air_time2 = arr_time-dep_time)


transmute(flights,
          arr_time,
          dep_time,
          air_time,
          dep_time_minutes = (60 * dep_time %/% 100 + dep_time %% 100) %% 1440 ,
          arr_time_minutes = (60 * arr_time %/% 100 + arr_time %% 100) %% 1440 ,
          air_time_minutes = arr_time_minutes - dep_time_minutes,
          air_time_diff=air_time-air_time_minutes)

check_air_time <- transmute(flights,
                            arr_time,
                            dep_time,
                            air_time,
                            dep_time_minutes = (60 * dep_time %/% 100 + dep_time %% 100) %% 1440 ,
                            arr_time_minutes = (60 * arr_time %/% 100 + arr_time %% 100) %% 1440 ,
                            air_time_minutes = arr_time_minutes - dep_time_minutes,
                            air_time_diff=air_time-air_time_minutes)

filter(check_air_time, air_time_diff!=0)
filter(check_air_time, air_time_diff %% 60==0)

ggplot(check_air_time, aes(x = air_time_diff)) +
  geom_histogram(binwidth = 1)

# This is odd. air_time_minutes still doesn't equal air_time. There could be an error with crossing time zones
# or arriving on a different date than departing, but both of those issues would lead to air_time_diff being off
# by a mulitple of 60 minutes. Only 942 of the 327,150 records with a air_time_diff!=0 have a air_time_diff
# divisible by 60. There must be some other input to the calculation of air_time than just 
# arr_time and dep_time.



## 5.5.2 3:

transmute(flights, 
          dep_time,
          sched_dep_time,
          dep_delay)

# dep_time should equal sched_dep_time (HHMM) + delay (in minutes).
# I'll convert to minutes from midnight for all variables then verify.

transmute(flights, 
          dep_time,
          sched_dep_time,
          dep_delay,
          dep_time_min = (60 * dep_time %/% 100 + dep_time %% 100) %% 1440 ,
          sched_dep_time_min= (60 * sched_dep_time %/% 100 + sched_dep_time %% 100) %% 1440 ,
          dep_time_check=sched_dep_time_min+dep_delay ,
          dep_time_diff = dep_time_check-dep_time_min)

filter(transmute(flights, 
                 dep_time,
                 sched_dep_time,
                 dep_delay,
                 dep_time_min = (60 * dep_time %/% 100 + dep_time %% 100) %% 1440 ,
                 sched_dep_time_min= (60 * sched_dep_time %/% 100 + sched_dep_time %% 100) %% 1440 ,
                 dep_time_check=sched_dep_time_min+dep_delay ,
                 dep_time_diff = dep_time_check-dep_time_min)
,dep_time_diff!=0)

check_dep_time <- filter(transmute(flights, 
                 dep_time,
                 sched_dep_time,
                 dep_delay,
                 dep_time_min = (60 * dep_time %/% 100 + dep_time %% 100) %% 1440 ,
                 sched_dep_time_min= (60 * sched_dep_time %/% 100 + sched_dep_time %% 100) %% 1440 ,
                 dep_time_check=sched_dep_time_min+dep_delay ,
                 dep_time_diff = dep_time_check-dep_time_min)
       ,dep_time_diff!=0)    

ggplot(check_dep_time, aes(x = dep_time_diff, y = dep_time )) +
  geom_point(binwidth = 1)  

# It appears that my hypothesis was correct. Dep_time is the sum of the 
# sched_dep_time and the dep_delay. The exception when trying to code
# this is when a dep_date occurs on the day after the flight was scheduled
# to depart. 


## 5.5.2 4:

# Tied records should share a rank that is the minimum rank of the 
# tied values. The min_rank function accomplishes this. 

head(arrange(transmute(flights,
          flights$flight,
          dep_delay,
          delay_rank = min_rank(desc(dep_delay)))
        ,delay_rank), 15)

# There were no ties in the top 10 delays, so this nuance 
# did not end up mattering, but we can see there is a 3-way tie for 
# 12th longest delay.



## 5.5.2 5:

1:3 + 1:10
x <- 1:3
y <- 1:10
x+y

# 1:3 + 1:10 returns an error message because R is not able to
# sum two vectors where the the length of the longer vector is not a 
# multiple of the length of the shorter vector.



## 5.6.7 2:

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  count(dest)

# Alternative:
not_cancelled %>%
  group_by(dest) %>%
  summarise(n=n())


not_cancelled %>%
  count(tailnum, wt = distance)

# Alternative:
not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n=sum(distance))





## 5.6.7 3:

# The most important column is the arr_delay. Flights can and do
# have missing arr_delay but non-missing dep_delay. These flights 
# could have crashed with no arrival time, so is.na(arr_delay) 
# captures that case.


## 5.6.7 4:

df1 <- 
  flights %>%
  mutate(cancelled= (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(flights=n(),
            cancelled_flights=sum(cancelled),
            cancelled_prop = mean(cancelled),
            avg_delay = mean(dep_delay,na.rm = TRUE)
            )

ggplot(df1,
       aes(x=as.factor(month),y=cancelled_prop))+
  geom_boxplot()


ggplot(df1,
       aes(x=avg_delay,y=cancelled_prop))+
  geom_point()

# There doesn't appear to be a relationship with cancelled flight
# proportion over time, but there is a positive relationship
# between avg_delay for a day and the rate of cancellation. 



## 5.6.7 5:

flights %>%
  group_by(carrier) %>%
  summarise(avg_delay=mean(dep_delay,na.rm = TRUE)) %>%
  arrange(desc(avg_delay))  

filter(airlines,carrier=="F9")

# Frontier Airlines has the longest average departure delay



## 5.6.7 6:

?count()
# The sort argument will sort the output in descending order of n if TRUE
# this argument could be used to save a step if we wanted to arrange
# an output in descending order.


## 5.7.1 1:

# When you combine the useful functions with a group, the function
# applies within each gorup instead of over the whole data frame.


## 5.7.1 2:

flights %>%
  filter(!is.na(arr_time) , !is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(flights=n(),avg_arr_delay=mean(arr_delay)) %>%
  arrange(desc(avg_arr_delay))

# Flight tailnumber N844MH has the worst on-time record with an average
# arrival delay of 320 minutes over its 1 flight.


## 5.7.1 3:

flights %>%
  filter(!is.na(arr_time) , !is.na(arr_delay)) %>%
  group_by(hour) %>%
  summarise(
    delay_count = sum(arr_delay>0),
    flight_cont = n(),
    delay_prop=sum(arr_delay>0)/n())

# It is best to plan for an early departure to avoid delay. The 5 am hour
# has the lowest arrival delay percentage, 29.0%



## 5.7.1 4:

flights %>%
  filter(!is.na(arr_time) , !is.na(arr_delay),arr_delay>0) %>%
  group_by(dest) %>%
  mutate(
    total_delay = sum(arr_delay,na.rm = TRUE),
    total_delay_prop= arr_delay/sum(arr_delay,na.rm=TRUE))


## 5.7.1 5: 

lag_delay <- flights %>%
  group_by(origin) %>%
  arrange(origin, time_hour) %>%
  mutate(prev_delay=lag(dep_delay))


lag_delay %>%
  filter(!is.na(dep_delay), !is.na(prev_delay)) %>%
  group_by(prev_delay) %>%
  summarise(avg_dep_delay=mean(dep_delay)) %>%
  ggplot(aes(x=prev_delay,y=avg_dep_delay)) + geom_point()

# There is a positive relationship between the depature delay of a flight and the 
# delay of the following flight. This relationship seems to be close to 
# linear for small delays, however, there appears to be a point at which 
# the delay is so large that it does not affect the next flight. I expect 
# in such a case, the airport knows the long delay is coming due to a 
# mechanical failure for example, and the next flight is able to stay on schedule.



## 5.7.1 6: 

# Suspiciously fast flights:
flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest,origin) %>%
  mutate(
    mean=mean(air_time),
    sd=sd(air_time),
    min=min(air_time),
    max=max(air_time),
    n=n()) %>%
  ungroup() %>%
  mutate(diff_to_min=air_time-min,
         perc_of_min=air_time/min-1,
         zscore=(air_time-mean)/sd) %>%
 select(flight,origin,dest, air_time,zscore,diff_to_min,perc_of_min, mean, sd, min, n) %>%
arrange(zscore)

# Most Delayed flights, relative to origin-destination distribution:

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest,origin) %>%
  mutate(
    mean=mean(air_time),
    sd=sd(air_time),
    min=min(air_time),
    max=max(air_time),
    n=n()) %>%
  ungroup() %>%
  mutate(diff_to_min=air_time-min,
         perc_of_min=air_time/min-1,
         zscore=(air_time-mean)/sd) %>%
  select(flight,origin,dest, air_time,zscore,diff_to_min,perc_of_min, mean, sd, min, n) %>%
  arrange(desc(zscore))



## 5.7.1 7: 

# all destinations with at least two carriers:
flights %>%
  group_by(dest) %>%
  summarise(carrier_count=n_distinct(carrier)) %>%
  filter(carrier_count>=2) %>%
  arrange(desc(carrier_count))

# I'll use a similar methodology to rank carriers based on how many destinations they fly to:

carrier_ranks <- flights %>%
  group_by(carrier) %>%
  summarise(dest_count=n_distinct(dest)) %>%
  arrange(desc(dest_count))

carrier_name <- merge(carrier_ranks,airlines,"carrier")
head(carrier_name,10)

# Endeavor Air Inc is the top airline by my ranking, travelling to 49 destinations.
# American Airlines and Alaska airlines are #2 and #3 respectively.





## 5.7.1 8: 


flights %>%
  filter(!is.na(dep_delay)) %>%
  arrange(tailnum,time_hour) %>%
  group_by(tailnum) %>%
  mutate( 
    n=n(),
    hour_delay = dep_delay>60) %>%
 mutate(find_delay = cumsum(hour_delay)) %>%
  filter(find_delay<1) %>%
  count(sort=TRUE)





## 19.3.1 1: 

verify_prefix <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
trim_last <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
match_length <- function(x, y) {
  rep(y, length.out = length(x))
}

# f1 should be called verify_prefix because it takes two strings and
# and checks to see if the second input is a prefix for the first input

verify_prefix("pretest","pre")

# f2 should be called trim_last because it trims the last value in 
# a vector. If there a vector is length 1, then trim_last will 
# return a null value

trim_last(c(1:10))

# f3 should be called match_length because it takes a value y and repeats it as many times 
# as the number of elements in vector x. The result is a vector with the same length as x
# but the elements will all be y.

f3(c(1,1,1),4)


## 19.3.1 2: 


# One function I have used and modified is sapply to check of missing
# values in a set of data frame columns. I think i could write
# a specific function to do this quicker. I'll call it check_for_na. 
# Now I only need to provide a single argument, the data frame I want to check.
# It is shown below:

sapply(popular_dests,function(x) sum(is.na(x)))
check_for_na <- function(x){
  sapply(x,function(x) sum(is.na(x)))
}
check_for_na(popular_dests)



## 19.3.1 3: 

?rnorm()
?MASS::mvrnorm()

# rnorm() takes a sample from a univariate normal distribution while
# mvrnorm() takes a sample from a multivariate normal distribution.
# Their arguments could use the same names to make them more consistent.


## 19.3.1 4: 


# The case for naming norm_?() would be that users would know they want 
# a function that relates to the normal distribution and then can choose
# the suffix needed from the autocomplete.
# However, it's better to keep the function as the prefix as it is 
# probably more likely that users know the function they nee then a distribution variant
# instead of the distribution then the function.


## 19.4.4 1: 

?ifelse()
?runif()
# If is for a single condition but ifelse() allows for checking all elements of
# a vector for a condition.


x <- c(1:10)
y <- ifelse(x %% 2 == 0, "Even", "Odd")

for (i in 1:length(x)) {
  if (x[i] %% 2 == 0) {
    y1[i] <- "Even"
  } else {
    y1[i] <- "Odd"
  } 
}

identical(y,y1)


## 19.4.4 2: 


greeting <- function(time=lubridate::now()) {
  if (lubridate::hour(time) < 12) {
    print("good morning") }
  else if (lubridate::hour(time) < 18) {
    print("good afternoon") }
  else { print("good evening")}
  }

greeting()


## 19.4.4 3: 


fizzbuzz <- function(x) {
  if (x%%15==0) {
    print("fizzbuzz")
  }
  else if (x%%3==0){
    print("fizz")
  }
  else if (x%%5==0){
    print("buzz")
  }
  else {print(x)}
}

fizzbuzz(1)
fizzbuzz(2)
fizzbuzz(3)
fizzbuzz(5)
fizzbuzz(15)
fizzbuzz(990)
fizzbuzz(998)


## 19.4.4 4: 

temp <- 20

if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}

?cut()

cut(temp,breaks=c(-Inf,0,10,20,30,Inf),labels=c("freezing","cold","cool","warm","hot"))

# If we used < instead of <=, we'd use:
cut(temp,breaks=c(-Inf,0,10,20,30,Inf),right=FALSE,labels=c("freezing","cold","cool","warm","hot"))

# If we have many values of temp, we can still use cut to 
# save us the time of writing a for loop:

temp1 <- seq(-10,50,10)
cut(temp1,breaks=c(-Inf,0,10,20,30,Inf),labels=c("freezing","cold","cool","warm","hot"))



## 19.4.4 5:

?switch()
switch(2,"blue","red","yellow")
switch(4,"blue","red","yellow")

# If switch is used with a numeric value, followed by a list
# the function will return the nth value in the list where
# n is the numeric argument provided in the switch function
# if n>length(list), then no value is returned


## 19.4.4 6:

switch("b", 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)
x <- "e"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)


# This function returns an error because x is not a vector of length 1 in my environment.
# If we set x="e", it will not return a value because "e" is not defined in the list.


