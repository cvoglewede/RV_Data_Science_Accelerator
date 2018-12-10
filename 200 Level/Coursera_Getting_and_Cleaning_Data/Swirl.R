library(swirl)
swirl()
Connor
2
1
mydf <- read.csv(path2csv,stringsAsFactors = FALSE)
dim(mydf)
head(mydf)
library(dplyr)
packageVersion("dplyr")

cran <- tbl_df(mydf)
rm("mydf")
cran
?group_by
?select
select(cran,ip_id,package,country)
5:20
select(cran,r_arch:country)
select(cran,country:r_arch)
cran
select(cran,-time)
-(5:20)
select(cran,-(X:size)) 
filter(cran,package=="swirl")

filter(cran, r_version == "3.1.1", country == "US")
?Comparison
filter(cran,country=="IN",r_version<="3.0.2")
filter(cran, country == "US" | country == "IN")
filter(cran,size>100500 , r_os=="linux-gnu")

is.na(c(3, 5, NA, 10))
!is.na(c(3, 5, NA, 10))

filter(cran,!is.na(r_version))
cran2 <- select(cran,size:ip_id)
arrange(cran2,ip_id)
arrange(cran2,desc(ip_id))
arrange(cran2, package, ip_id)
arrange(cran2, country, desc(r_version), ip_id)

cran3 <- select(cran,ip_id,package,size)
cran3
mutate(cran3, size_mb = size / 2^20)
mutate(cran3, size_mb = size / 2^20,size_gb=size_mb/2^10)
mutate(cran3,correct_size=size+1000)

summarize(cran, avg_bytes = mean(size))

by_package <- group_by(cran,package)
by_package

summarise(by_package,mean(size))
quantile(pack_sum$count, probs = 0.99)

top_counts <- filter(pack_sum,count>679)
top_counts
View(top_counts)


top_counts_sorted <- arrange(top_counts,desc(count))
View(top_counts_sorted)


quantile(pack_sum$unique, probs = 0.99)

top_unique <- filter(pack_sum,unique>465)
View(top_unique)
top_unique_sorted <- arrange(top_unique,desc(unique))
View(top_unique_sorted)
View(result3)
submit()



library(tidyr)
6
students
?gather
gather(students,sex,count,-grade)
students2
res <- gather(students2,key=sex_class,value=count,-grade)
res
?separate

separate(res,col=sex_class,into=c("sex","class"))

submit()
students3
?spread
library(readr)
parse_number("class5")

students4
submit()
passed
failed
passed <- passed %>% 
  mutate(status="passed")
failed <- failed %>% 
  mutate(status="failed")


bind_rows(passed,failed)
sat
submit()
2
