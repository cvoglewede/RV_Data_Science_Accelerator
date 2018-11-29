library(RMySQL)


# lecture notes

ucsdb <- dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")

result <- dbGetQuery(ucsdb,"show databases;");dbDisconnect(ucsdb);

#H5 Files

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)

created = h5createFile("example.h5")
created = h5createGroup("example.h5","foo")
created = h5createGroup("example.h5","baa")
created = h5createGroup("example.h5","foo/foobaa")
h5ls("example.h5")

A=matrix(1:10,nr=5,nc=2)
h5write(A,"example.h5","foo/A")
B=array(seq(.1,2,by=.1),dim=c(5,2,2))
attr(B,"scale") <- "liter"
h5write(B,"example.h5","foo/foobaa/B")
h5ls("example.h5")
df=data.frame(1L:5L,seq(0,1,length.out = 5),
                        c("ab","cde","fghi","a","s"),
                        stringsAsFactors =FALSE)
h5write(df,"example.h5","df")
h5ls("example.h5")

readA=h5read("example.h5","foo/A")
readB=h5read("example.h5","foo/foobaa/B")
readdf=h5read("example.h5","df")
readA

h5write(c(12,13,14),"example.h5","foo/A",index=list(1:3,1))
h5read("example.h5","foo/A")


# Reading from the Web

con=url("https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlcode=readLines(con)
close(con)
htmlcode
url_xml <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
url_curl <- getURL(url_xml)
html_xml <- htmlTreeParse(url_curl,useInternalNodes = TRUE)
html_xml
xpathSApply(html_xml,"//title",xmlValue)
xpathSApply(html_xml,"//td[@id='col-citedby']",xmlValue)

html2=GET(url_xml)
content2=content(html2,as="text")
parsedhtml=htmlParse(content2,asText=TRUE)
xpathSApply(parsedhtml,"//title",xmlValue)
parsedhtml

# websites with passwords:

pg2=GET("http:xxxxx",authenticate("user","password"))

# handles

google=handle("http://google.com")
pg1=GET(handle=google,path="/")
pg2=GET(handle=google,path="search")

# For more help on web scraping:
# R bloggers, search Web+Scraping
# httr package page



# Getting Data from APIs:

# xx=GET(url,<parameters>)
# json1=content(xx)
# json2=fromJSON(toJSON(json1))
# json2[1,1:4]


# Quiz:

# 1:
oauth_endpoints("github")
my_app <- oauth_app(appname = "Coursera_API_Test"
                    ,key = "b68b43c4b7f65eff8dae"
                    ,secret = "ae70f53e0909088b72a95aba615a8e313faf6369")

github_token <- oauth2.0_token(oauth_endpoints("github"),my_app)

gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)

json1 = content(req)
gitDF = fromJSON(toJSON(json1))

gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 


# 2

library("sqldf")
acs <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv")
head(acs)

sqldf("select pwgtp1 from acs")
sqldf("select pwgtp1 from acs where AGEP \lt< 50")

# 3

unique(acs$AGEP)
sqldf("select AGEP where unique from acs")
# no
sqldf("select distinct pwgtp1 from acs")
# prob not
sqldf("select distinct AGEP from acs")
# yes
sqldf("select unique * from acs")
# no

# 4
con=url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlcode=readLines(con)

htmlcode
htmlcode[c(10,20,30,100)]
nchar(htmlcode[c(10,20,30,100)])


# 5

library("foreign")
library(data.table)

df5 <- read.fwf("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
         # ,header = 
        ,widths = c(10,-5,4,-1,3,-5,4,-1,3,-5,4,-1,3,-5,4,-1,3)
        ,skip=4
        )

head(df5)

sum(df5$V4)
