## Connor Voglewede
## Week 2 R Homework
## 4.4 1-3 , 
## 5.2.4 1-4 ,
## 5.3.1 1-4 ,
## 5.4.1 2-4


## 4.4 1:

# The number 10 is assigned to to my_variable, spelled with an i. 
# The next line, my_var1able is a typo, with a dotless 'i' or '1' where the dotted 'i' should be. 
# This is why the code returns an error.


## 4.4 2:


install.packages("tidyverse")
library(tidyverse)
# library(tidyverse) must be prefaced with install.packages("tidyverse") for instances in which tidyverse is not pre-installed.

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# This code works just fine for me and shows a scatterplot depicting the negative correlation between the variables hwy and displ.

# fliter(mpg, cyl = 8)
filter(mpg, cyl == 8)
# There are two errors that must be corrected. First, there is the typo on the function filter. Second, "==" needs to be used instead of "=". 

# filter(diamond, carat > 3)
filter(diamonds, carat > 3)
# There is a typo on the dataset name. diamonds os the correct data frame.




## 4.4 3:

# A shortcut menu pops up when I press Alt + Shift + K.
# Through the menus, the keyboard shortcuts can be accesed through Tools -> Keyboard Shortcuts Help.




## 5.2.4 1:

install.packages("nycflights13")
library(nycflights13)

#a
filter(flights, arr_delay >= 120)

#b
filter(flights, dest == "IAH" | dest == "HOU")

#c
filter(flights, 
              carrier == "UA" |
              carrier == "AA" | 
              carrier == "DL")

#d
filter(flights, month %in% c(7,8,9))

#e
filter(flights, dep_delay <= 0, arr_delay > 120)

#f
filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)

#g
filter(flights, dep_time <= 600 | dep_time == 2400)


## 5.2.4 2:

# between() is a helpful function to specify an inclusive range of values. The code for 1, part d could be simplified with the following:
filter(flights, between(month,7,9))


## 5.2.4 3:

sum(is.na(flights$dep_time))
# 8,255 flights have a missing departure time

look_for_missing <- filter(flights,is.na(dep_time))
sapply(look_for_missing,function(x) sum(is.na(x)))
# departure delay, arrival time, arrival delay, and air time are all missing values of every record where departure time is missing. These are probably cancelled flights.


## 5.2.4 4:

NA^0
NA | TRUE
FALSE & NA
NA * 0

# NA^0==1 because all numbers raised to the 0 power equals 1. 
# NA | TRUE is TRUE because TRUE is TRUE and TRUE | x is going to be TRUE for any x
# FALSE & NA is FALSE because FALSE and anything will be FALSE
# If the NA matters, then the result will be 'NA'. 
# In the NA * 0 example, it returns NA as the result even though you might think that x*0==0 for all x. However, that's not true if x=+/- infinity. In such a case, Inf*0 returns NaN. Thus the NA matters and NA*0 returns NA.


## 5.3.1 1:

# missing values first
arrange(flights, desc(is.na(dep_time)))

## 5.3.1 2:

# Most delayed flights
arrange(flights, desc(dep_delay))

# Smallest delay
arrange(flights, dep_delay)


## 5.3.1 3:

# Fastest Flights
arrange(flights, air_time)


## 5.3.1 4:

# Longest Flights
arrange(flights, desc(air_time))

# Shortest Flights
arrange(flights, air_time)


## 5.4.1 2:

select(flights, carrier, carrier, air_time, dep_delay)
# Duplicating variables doesn't not affect the output of the select function.


## 5.4.1 3:

# one_of() is a function that allows for a list of character strings to be references as a vector, instead of many separate character strings. For example, we can use the select() function with one_of() to return a data frame with column names that are a part of the vector vars.
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))


## 5.4.1 4:

select(flights, contains("TIME"))
# This result is a bit surprising in the sense that case does seem to matter to the select() helper function.
# To change the default of ignoring case, you can add the argument ignore.case = FALSE in the contain function after "TIME".

select(flights, contains("TIME", ignore.case = FALSE))


