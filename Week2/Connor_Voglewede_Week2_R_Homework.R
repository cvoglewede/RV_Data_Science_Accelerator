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

# This code works just fine for me and shows a scatterplot depicting the negative correlation between the variables hwy and displ 

# fliter(mpg, cyl = 8)
filter(mpg, cyl == 8)
# There are two errors that must be corrected. First, there is the typo on the function filter. Second, "==" needs to be used instead of "=" 

# filter(diamond, carat > 3)
filter(diamonds, carat > 3)
# There is a typo on the dataset name. diamonds os the correct data frame.
