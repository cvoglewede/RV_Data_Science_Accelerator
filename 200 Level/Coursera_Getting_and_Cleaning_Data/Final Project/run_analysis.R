library("dplyr")
library("readr")


# Importing Data

filename <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
t1 = tempdir()
tf = tempfile(tmpdir=t1, fileext=".zip")
download.file(filename, tf)
unzip(tf, exdir=t1, overwrite=TRUE)


feature_labels <- read.table(file.path(t1, "UCI HAR Dataset/features.txt"))
activity_labels <- read_table(file.path(t1, "UCI HAR Dataset/activity_labels.txt"),col_names = FALSE)
y_train <- read_table(file.path(t1, "UCI HAR Dataset/train/y_train.txt"),col_names = FALSE)
x_train <- read_table(file.path(t1, "UCI HAR Dataset/train/x_train.txt"),col_names = FALSE)
subject_train <- read_table(file.path(t1, "UCI HAR Dataset/train/subject_train.txt"),col_names = FALSE)
y_test <- read_table(file.path(t1, "UCI HAR Dataset/test/y_test.txt"),col_names = FALSE)
x_test <- read_table(file.path(t1, "UCI HAR Dataset/test/x_test.txt"),col_names = FALSE)
subject_test <- read_table(file.path(t1, "UCI HAR Dataset/test/subject_test.txt"),col_names = FALSE)



# Organizing Data
feature_labels[,3] <- paste(feature_labels$V1," ",feature_labels$V2) 
colnames(feature_labels)=c("Index","Feature","Unique Feature")
colnames(activity_labels)=c("Index","activity")
colnames(y_train)=c("Y_Label")
colnames(subject_train)=c("Subject_Label")
colnames(y_test)=c("Y_Label")
colnames(subject_test)=c("Subject_Label")
colnames(x_test) <- (feature_labels[,3])
colnames(x_train) <- (feature_labels[,3])


# Consolidating Data
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


# Subsetting Data
mean_cols_boolean <- grepl("mean()",fixed=TRUE,colnames(Consolidated))
sd_cols_boolean <- grepl("std()",fixed=TRUE,colnames(Consolidated))
mean_cols <- colnames(Consolidated[mean_cols_boolean])
sd_cols <- colnames(Consolidated[sd_cols_boolean])

Trimmed <- Consolidated %>% 
  select(1:4,mean_cols,sd_cols)


# Check for NA values across columns
max(sapply(Trimmed, function(x) sum(is.na(x))))


# Summarized data
Means_by_Subject <- Trimmed %>% 
  group_by(Subject_Label) %>% 
  summarise_all(funs(mean)) %>% 
  select(Subject_Label,5:70)


Means_by_Activity <- Trimmed %>% 
  group_by(activity) %>% 
  summarise_all(funs(mean)) %>% 
  select(activity,5:70)


Means_by_Subject_Activity <- Trimmed %>% 
  group_by(Subject_Label,activity) %>% 
  summarise_all(funs(mean)) %>% 
  select(Subject_Label,activity,5:70)


# Tidy Dataset
TidyDataset <- Means_by_Subject_Activity


# Write Tidy Table
write.table(TidyDataset,"Tidy Dataset.txt",row.names = FALSE)



