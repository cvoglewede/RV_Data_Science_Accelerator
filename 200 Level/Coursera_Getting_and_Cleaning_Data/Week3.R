
# Lecture Notes

set.seed(13435)

x <- data_frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
x <- x[sample(1:5),]
x$var2[c(1,3)]=NA
x

x[,1]

x[,"var1"]

x[1:2,"var2"]

x[x$var1<=3 & !is.na(x$var2),]

x[which(x$var2>8),]


sort(x$var2,decreasing = TRUE,na.last = TRUE)

x[order(x$var1),]
arrange(x,var1)

x$var4 <- rnorm(5)

Y <- cbind(x,rnorm(5))
Y


# quantile()
# quantile(probs=c(.5,.75.9))

# table(useNA="ifany")
# table(x1,x2) -> matrix of count of interactions

# any(logical)

# all(logical)

colSums(is.na(x))

# table(logical) -> true false table!

# x[logical] -> subset x with logical

# xtabs(variable ~ variable + variable, data=x)

# xtab <- xtabs(variable ~.,data=x)
# ftable(xtab) -> flatten out multiple xtabs


object.size(x)
print(object.size(x),units = 'Kb')


# ifelse(logical, v1,v2)

# cut(x,breaks=list of values)
# cut2(variable,g=n)


# factor(variable,levels=list)


# sample(list of options,size=n, replace=TRUE)


mtcars$carname=rownames(mtcars)
carmelt <- melt(mtcars,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp"))

# identify id variables and measure variables

dcast(carmelt,cyl~variable)
# summarize by length

dcast(carmelt,cyl~variable,mean)
# summarize by mean


attach(InsectSprays)
tapply(count,spray,sum)
# to var 1, by var 2, do function "sum"

split(count,spray)
# split by var 2, full list of var 1

lapply(split(count,spray), sum)
# apply to list function "sum"

unlist(lapply(split(count,spray), sum))
# combine the lapply output

InsectSprays %>% 
  group_by(spray) %>% 
  mutate(sum=sum(count))




# merge(x,y,by.x = ,by.y=,all=TRUE)

# intersect(names(x),names(y))
# See the columns present in both x and y dataframes


######
# QUIZ
######


# 1

df1 <- read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")


attach(df1)

df1[,"ACR"]

agricultureLogical <- (df1$ACR==3 & df1$AGS==6)
head(agricultureLogical)
which(agricultureLogical)

df1[125,c("ACR","AGS")]


# 2


file <- "http://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
path <- file.path(getwd(),"jeff.jpg")
file1 <- download.file(file,path,mode="wb")
jpeg2 <- readJPEG(path,native=TRUE)

head(jpeg2)
quantile(jpeg2,probs=c(.3,.8))


# 3

df3a <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",skip = 4,nrows=215)

df3b <- read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv")


setnames(df3a,c("X"),c("CountryCode"))


df3 <- merge(df3a,df3b,all=TRUE,by="CountryCode")
df3

sum(!is.na(unique(df3$`Long Name`)))


 ranking <-  df3 %>% 
  arrange(desc(X.1))

 ranking[13,"Long Name"]




# 4
 
 df3 %>% 
   filter(`Income Group` %in% c("High income: OECD","High income: nonOECD")) %>% 
   group_by(`Income Group`) %>% 
   summarise(mean=mean(X.1,na.rm = TRUE))

 
 # 5
 
 
 df5 <-  df3 %>% 
   filter(X.1>0) %>%
   mutate(group=cut2(X.1,g=5))


df5 %>% 
  group_by(group,`Income Group`) %>% 
  summarise(count=length(CountryCode))
 
 
 
 
 
 
 
 
 
 



