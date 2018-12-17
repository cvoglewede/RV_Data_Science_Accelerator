
# Quiz

# 1

df1 <- read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")

head(df1)

names(df1)[123]
strsplit(names(df1),"wgtp")[123]
  
# 2

df2 <- read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",skip=4)

head(df2)

df2 %>% 
  filter(!is.na(X2)) %>% 
mutate(gdp=as.numeric(gsub(",","",X5))) %>% 
summarize(mean=mean(gdp,na.rm = TRUE))


# 3

df3 <- df2 %>% 
  filter(!is.na(X4))

grep("United",X4,value = TRUE)

grep("United$",X4,value = TRUE)
grep("^United",X4,value = TRUE)
grep("*United",X4,value = TRUE)
grep("*United",X4,value = TRUE)


# 4

df4 <- read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",skip = 4)

df4_join <- read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv")

head(df4_join)

df4_notes <- df4 %>% 
  filter(!is.na(X1)) %>% 
  inner_join(df4_join,by=c("X1"="CountryCode")) %>% 
  rename("Notes"="Special Notes") %>% 
  select("X4","Notes")



sum(grepl("[Ff]iscal year end: June",df4_notes$Notes))


# 5

install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)


sum(year(sampleTimes)==2012)
# 250

sum(weekdays(sampleTimes)=="Monday" & year(sampleTimes)==2012 )








