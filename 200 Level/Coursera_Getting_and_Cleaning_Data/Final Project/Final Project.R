library("dplyr")
library("ggplot2")
library("readr")
library("data.table")
library("readxl")
library("XLConnect")
library("RMySQL")
library("jsonlite")
library("tidyr")
library("lubridate")
library("stringr")
library("tidyverse")



setwd("/Users/cvoglewede/Downloads/UCI HAR Dataset 3/")


feature_labels <- read.table("features.txt")
feature_labels[,3] <- paste(feature_labels$V1," ",feature_labels$V2) 
colnames(feature_labels)=c("Index","Feature","Unique Feature")

activity_labels <- read_table("activity_labels.txt",col_names = FALSE)
colnames(activity_labels)=c("Index","activity")

setwd("/Users/cvoglewede/Downloads/UCI HAR Dataset 3/train/")
y_train <- read_table("y_train.txt",col_names = FALSE)
colnames(y_train)=c("Y_Label")

x_train <- read_table("x_train.txt",col_names = FALSE)

subject_train <- read_table("subject_train.txt",col_names = FALSE)
colnames(subject_train)=c("Subject_Label")


setwd("/Users/cvoglewede/Downloads/UCI HAR Dataset 3/test/")

y_test <- read_table("y_test.txt",col_names = FALSE)
colnames(y_test)=c("Y_Label")

x_test <- read_table("x_test.txt",col_names = FALSE)

subject_test <- read_table("subject_test.txt",col_names = FALSE)
colnames(subject_test)=c("Subject_Label")


colnames(x_test) <- (feature_labels[,3])
colnames(x_train) <- (feature_labels[,3])



Training <- subject_train %>% 
  cbind(y_train) %>% 
  left_join(activity_labels,by=c("Y_Label"="Index")) %>% 
  mutate(Dataset = "Train") %>% 
  cbind(x_train)
  
Test <- subject_test %>% 
  cbind(y_test) %>% 
  left_join(activity_labels,by=c("Y_Label"="Index")) %>% 
  mutate(Dataset = "Test") %>% 
  cbind(x_test)

Consolidated <- Training %>% 
  rbind(Test)


mean_cols_bool <- grepl("mean()",fixed=TRUE,colnames(Consolidated))
sd_cols_bool <- grepl("std()",fixed=TRUE,colnames(Consolidated))

mean_cols <- colnames(Consolidated[mean_cols_bool])
sd_cols <- colnames(Consolidated[sd_cols_bool])

Trimmed <- Consolidated %>% 
  select(1:4,mean_cols,sd_cols)


max(sapply(Trimmed, function(x) sum(is.na(x))))
colnames(Trimmed)[!complete.cases(t(Trimmed))]

# col_means <- sapply(Trimmed[5:70],function(x) mean(x))
# col_sd <- sapply(Trimmed[5:70],function(x) sd(x))


Means_by_Subject <- Trimmed %>% 
  group_by(Subject_Label) %>% 
  summarise_all(funs(mean)) %>% 
  select(Subject_Label,5:70)

Means_by_Subject

Means_by_Activity <- Trimmed %>% 
  group_by(activity) %>% 
  summarise_all(funs(mean)) %>% 
  select(activity,5:70)

Means_by_Activity

Means_by_Subject_Activity <- Trimmed %>% 
  group_by(Subject_Label,activity) %>% 
  summarise_all(funs(mean)) %>% 
  select(Subject_Label,activity,5:70)

Means_by_Subject_Activity







