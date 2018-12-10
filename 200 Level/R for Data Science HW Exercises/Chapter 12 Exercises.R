library(tidyverse)

# 12.2.1 #2-3
# 12.3.3 #1-4
# 12.5.1 #1-2


# 12.2.1 2

table2 <- table2
table4a <- table4a
table4b <- table4b

head(table2)
head(table4a)
head(table4b)


cases <- table2 %>% 
  filter(type=="cases") %>%
  rename(cases="count") %>%
  arrange(country,year)

population <- table2 %>% 
  filter(type=="population") %>%
  rename(population="count") %>%
  arrange(country,year)


merged <- bind_cols(cases,population)

merged %>%
  mutate(rate=cases/population*1000) %>%
  select(country,year,rate)



a <- table4a %>%
  gather('1999','2000',key="year",value="cases")
b <- table4b %>%
  gather('1999','2000',key="year",value="population")
a
b
merged <- bind_cols(a,b)
merged
merged %>%
  mutate(rate=cases/population*1000) %>%
  select(country,year,rate)

a %>%
  left_join(b,by=c("country"="country","year")) %>%
  mutate(rate=cases/population*1000)
  

# I found it easier working with the two separate tables 4a and 4b to bind together to calculate the rate.


# 12.2.1 3

ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

new_plot <- merged %>%
  mutate(rate=cases/population*1000) %>%
  select(country,year,cases)

ggplot(new_plot, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

# I needed to take the merged table from question 2, select cases instead of the rate, and use that tibble as the data for the ggplot


# 12.3.3 1

stocks

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

stocks %>% 
  spread(year, return) 
  
str(stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`,convert = TRUE))

str(stocks)

# Spread and gather are not perfectly symmetrical because data type is not transferred when spreading and gathering.Gathering always coerces the target variable into a character vector even though "year" is numeric in stocks and after spreading.
# The convert argument allows for spread and gather to be closer to symmetrical by guessing the data type of the target variable when gathering. It can still be different than the original data type, as seen in this example ("int" vs "character"), but it can get closer.



# 12.3.3 2

table4a %>%
  gather(1999, 2000, key = "year", value = "cases")

table4a %>% 
  gather('1999', '2000', key = "year", value = "cases")

# The code fails we're attempting to gather variables with names 1999 and 2000, not the 199th and 200th column of the table4a data frame. Quote will tell gather to look for that specific column name.



# 12.3.3 3


people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people

people %>%
  spread(key,value)

# There are two duplicate identifiers for rows 1 and 3. Looking at the data, this is obviously true. Phillip Wood is listed with two ages, 45 and 50. We'd be unable to choose which age to list when we spread this data so there is an error

people_new <-
  people %>%
  group_by(name,key) %>%
  mutate(order=order(value))

people_new %>%
  spread(key,value)

# You could add an ordered log of the entries and spread on the order, in addition to the name.



# 12.3.3 4

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
preg

preg_new <- preg %>% 
  gather('male','female',key="sex",value="count")

# The variables are pregnant, sex, and a count of instances




# 12.5.1 1

stocks %>% 
  spread(year,return) %>% 
  gather(year, return,'2015':'2016')

args(fill)
args(spread)
args(complete)

# Fill is listed as an argument itself of both spread and complete, but it is not the same functionality of the fill function. 
# In spread, the fill argument default is NA. tidyverse will fill in missing values with the NA that we've seen in the examples unless you specific the value that spread will fill in for missing values
# In complete, the fill argument will fill in the missing values with with the missing elements of the columnar combinations.

# 12.5.1 2

# The direction argument to fill() allows the user to specify whether they want missing values to be pulled "down" from the preceding value or pulled "up" from the subsequent entry.

