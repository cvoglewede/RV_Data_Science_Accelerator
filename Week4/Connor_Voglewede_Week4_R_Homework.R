## Connor Voglewede
## Week 3 R Homework
# 20.3.5 #1-4
# 20.4.6 #1-6
# 20.5.4 #1-2
# 21.2.1 #1-4
# 21.3.5 #1-3
# 21.5.3 #1


## 20.3.5 1:

x <- c(1,Inf,-Inf,NA,NaN)

is.finite(x)
!is.infinite(x)
is.finite(x)==!is.infinite(x)

# is.finite(x) is the same as !is.infinite(x) except when 
# for values of x that are undefinted like NA and NaN.
# In those cases, is.finite(x) returns a value of false
# while !is.infinite(x) returns a value of true.


## 20.3.5 2:

dplyr::near
.Machine$double.eps^0.5
dplyr::near(3,3.1)
dplyr::near(3,3.0000000000000001)

# dplyr::near() is a function that evaluates whether two values have an absolute value difference
# of less than 1.5 * 10^-8, or basically 0.
# You can see that 3 and 3.1 are not 'near' by this definition
# but 3 and 3.0000000000000001 are near by this definition.


## 20.3.5 3:


?integer
?double
.Machine$integer.max
.Machine$double.xmax

# An integer vector can take on 2*2147483647+1. 2147483647 because it is the max integer valuet that R can represent multiplied by 2 to account 
# for both positive and negative values. We need to add 1 to account the the value NA.

# A double can take 2*(2*10^308) values. The max value a double can take is 2*10^308 and we need to multiply by two
# to account for both positive and negative numbers.




## 20.3.5 4:

typeof(5)
typeof(strtoi(floor(5)))
typeof(as.integer(5))
typeof(type.convert(5,"integer"))
typeof(integer(5))




## 20.4.6 1:

x <- c(1,2,NA,5, Inf)
mean(is.na(x))
# This function returns the proportion of elements in vector x that have the value NA

sum(!is.finite(x))
# This function returns the number of elements in vector x that has a TRUE value for the condition !is.finite




## 20.4.6 2:


?is.vector
# is.vector tests for whether or not a vector x has only names as elements

?is.atomic
# is.atomic doesn't care if all elements in a vector are one of the atomic types or null
# The elements can be any mismatch of atomic vector types.
# if an element of a vector is a list, then is.atomic will return FALSE


## 20.4.6 3:

?setNames
?purrr::set_names

# Both functions allow the user to assign names to an object in a vector
# setNames takes two arguments, the first a vector of elements and second a vector of names for those elements
# purr::set_names allows for the names to be assigned as sequential arguments


## 20.4.6 4:

# 1
last_elem <- function (x){
  x[length(x)]
}
x <- c(1,2,3,4,5,6)
last_elem(x)

# 2
even_position <- function(x){
  x[seq(2,length(x),2)]
}
even_position(x)

# 3
except_last_elem <- function(x){
  x[seq(1,length(x)-1,1)]
}
except_last_elem(x)

# 4
even_elem <- function(x){
  x[x%%2==0]
}
even_elem(x)
y <- c(2,2,2,5,5,10,12)
even_elem(y)


## 20.4.6 5:

x <- c(1,0,-1,Inf,-Inf,NA,NaN)
x[-which(x > 0)]
x[x <= 0]

# The two expressions are differnet in their treatment of NaN values. x[-which(x > 0)] returns a NaN value while x[x<=0] returns a NA value.



## 20.4.6 6:

x <- c(1,2,3)
x[4]

# If you subset a vector with an interger larger than the length of the vector, R returns a NA value

x1 <- purrr::set_names(x,"a","b","c")
x1["a"]
x["d"]

# Likewise, if you subset a vector with a name that does not exist, R returns a NA value



## 20.5.4 1:

# Literally draw them?..


## 20.5.4 2:

x <- tibble::as_tibble(c(1:5))
x1 <- list("A"=c(1:5))
colnames(x) <- "A"

x["A"]
x1["A"]

# Subsetting a list works the same as subsetting a list.
# A list can have variable length by element.
# A tibble must have consistent length by column.


## 21.2.1 1:

#1 

means <- vector("numeric",ncol(mtcars))
names(means) <- names(mtcars)
for (i in names(mtcars)){
 means[i] <- mean(mtcars[[i]])
}
print(means)

#2 

library(nycflights13)
types <- vector("list",ncol(flights))
for (i in names(flights)){
  types[i] <- class(flights[[i]])
}
print(types)

#3

uniques <- vector("numeric",ncol(iris))
names(uniques) <- names(iris)
for (i in names(iris)){
  uniques[i] <- length(unique(iris[[i]]))
}
print(uniques)

#4

mu <- c(-10,0,10,100)
random_list <- vector("list",length(mu))
names(random_list) <- c(-10,0,10,100)
for (i in c(1:4)){
  random_list[[i]] <- rnorm(10,mean=mu[i])
}
random_list



## 21.2.1 2:

out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
out
# Alternative:
stringr::str_c(letters,collapse = "")


x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
x
sd
# Alternative:
sd(x)


x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
x
out
# Alternative:
cumsum(x)


## 21.2.1 3:

#1
# lyrics: https://www.songsforteaching.com/folk/alicethecamel.php

hump <- c("five humps","four humps","three humps","two humps","one hump","no humps")

for (i in hump){
  cat(stringr::str_c("Alice the camel has ",rep(i,3),".",
                     collapse="\n"),
      "\n")
 if (i == "no humps") {
    cat("Because Alice is a horse, of course!\n")
  } 
  else {
    cat("So ride, Alice, ride. Boom, boom!\n")
  }
  cat("\n")
}


#2

 # lyrics: https://kidsongs.com/lyrics/ten-in-the-bed.html/

num_in_bed <- c("ten","nine","eight","seven","six","five","four","three","two","one")

for (i in num_in_bed){
  cat(stringr::str_c("There were ",i," in the bed\n"))
  cat("and the little one said\n")
  if (i=="one"){
    cat("'Alone at last!'")
  }
  else {
    cat("'Roll over! Roll over!'\n")
    cat("So they all rolled over and one fell out\n")
  }
  
}


# Generalized solution:
   How_many_in_bed <- function(x){ 
  for (i in seq(x,1)){
    cat(stringr::str_c("There were ",i," in the bed\n"))
    cat("and the little one said\n")
    if (i==1){
      cat("'Alone at last!'")
    }
    else {
      cat("'Roll over! Roll over!'\n")
      cat("So they all rolled over and one fell out\n")
    }
    
  }}  
  How_many_in_bed(25)
  
#3

  # lyrics: https://en.wikipedia.org/wiki/99_Bottles_of_Beer
  
  
bottle_text <- function(i){
  if (i>2){
    bottle_text <- stringr::str_c(i-1," bottles")
  }
  else if (i==2) {
    bottle_text <- "1 bottle"
  }
  else {
    bottle_text <- "No more bottles"
  }
  bottle_text
}

library(stringr)
How_many_bottles <- function(x){
  for (i in seq(x,2)){
    cat(str_c(bottle_text(i)," of beer on the wall, ",bottle_text(i)," of beer.\n"))
    cat(str_c("Take one down and pass it around, ",bottle_text(i-1)," of beer on the wall.\n"))
  }
  cat("No more bottles of beer on the wall, No more bottles of beer.\n")
  cat("We've taken them down and passed them around; now we're drunk and passed out!\n")
}

How_many_bottles(10)



## 21.2.1 4:

output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}
output

# There's a R package called microbenchmark that can time function runtime. I'll use that to compare my example function below:

library(microbenchmark)

undefined_vector <- function(x){
  output <- vector("integer",0)
  x1=c(1:100)
  for (i in seq_along(x1)) {
    output <- c(output, sum(x1[[i]]))
  }
}

defined_vector <- function(y){
  output <- vector("integer",y)
  y1=c(1:100)
  for (i in seq_along(y1)) {
    output <- c(output, sum(y1[[i]]))
  }
}

microbenchmark(undefined_vector(10000), times = 300)
microbenchmark(defined_vector(10000), times = 300)

# The undefined vector function takes about 68 nanoseconds on average while the defined vector function takes about 4% of that time, about 3 nanoseconds on average.



## 21.3.5 1:

files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)
for (i in 1:length(files)){
  df[[i]] <- read.csv(files[[i]])
}
library(dplyr)
df_all <- bind_rows(df)


## 21.3.5 2:


# If x has no names, then the for loop will not run
# If only some of x is named, then then R will return an error when the loop gets to that element
# If names are not unique, R will always return the first record of the unique name.



## 21.3.5 3:

show_mean <- function(x){
  for (i in names(x)){
    if (is.numeric(x[[i]])){
      cat(str_c(i,": ",format(mean(x[[i]]),digits=3),"\n"))
    }
  }
}

show_mean(iris)


## 21.5.3 1:


#1
library(purrr)
map_dbl(mtcars,mean)

#2
map_chr(flights,typeof)

#3
map_int(iris, ~length(unique(.)))

#4
mu <- c(-10, 0, 10, 100)
map(mu, ~ rnorm(n = 10, mean = .))



