# Codebook run_analysis.R
#### Author: Connor Voglewede
#### December 17, 2018


## Raw Data

Full details about the Human Activity Recognition Using Smartphones data set is available here:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

For run_analysis.R, I only use the following datasets from UCI:

- features.txt: List of measurement feature names
- activity_labels.txt: Activity Names
- Training data:
	- train/x_train.txt: measurements for training data
	- train/y_train.txt: activity labels for training data
	- train/subject_train.txt: subject identifier for training data
- Test data:
	- test/x_test.txt: measurements for test data
	- test/y_test.txt: activity labels for test data
	- test/subject_test.txt: subject identifier for test data


## R Requirements

The following R packages are required to run run_analysis.R

- dplyr
- readr


## Importing Data

The first step is to download the zip file from the UCI website in a reproducible manner. I use the following to read in the zip and unzip the many .txt files within:


`filename <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"`
`t1 = tempdir()`
`tf = tempfile(tmpdir=t1, fileext=".zip")`
`download.file(filename, tf)`
`unzip(tf, exdir=t1, overwrite=TRUE)`


This brings in all of the files. I won't use all of them but it's good to have them available to me here.

Next, I'll read in the .txt files I do need:

- feature.txt
- activity_labels.txt
- y_train.txt
- x_train.txt
- subject_train.txt
- y_test.txt
- x_train.txt
- subject_test.txt

I use a combination of read_table and read.table to read this data into dataframes. Features.txt was particularly difficult to read correctly with its uniquely structured variable, so I used read.table for that data source to split the variable into two columns in `feature_labels`


`feature_labels <- read.table(file.path(t1, "UCI HAR Dataset/features.txt"))`
`activity_labels <- read_table(file.path(t1, "UCI HAR Dataset/activity_labels.txt"),col_names = FALSE)`
`y_train <- read_table(file.path(t1, "UCI HAR Dataset/train/y_train.txt"),col_names = FALSE)`
`x_train <- read_table(file.path(t1, "UCI HAR Dataset/train/x_train.txt"),col_names = FALSE)`
`subject_train <- read_table(file.path(t1, "UCI HAR Dataset/train/subject_train.txt"),col_names = FALSE)`
`y_test <- read_table(file.path(t1, "UCI HAR Dataset/test/y_test.txt"),col_names = FALSE)`
`x_test <- read_table(file.path(t1, "UCI HAR Dataset/test/x_test.txt"),col_names = FALSE)`
`subject_test <- read_table(file.path(t1, "UCI HAR Dataset/test/subject_test.txt"),col_names = FALSE)`


## Organizing 

Here I'm going to create a unique feature identifier column in `feature_labels` and reassign column names where appropriate to make my future joins more intuitive.


`feature_labels[,3] <- paste(feature_labels$V1," ",feature_labels$V2)`
`colnames(feature_labels)=c("Index","Feature","Unique Feature")`
`colnames(activity_labels)=c("Index","activity")`
`colnames(y_train)=c("Y_Label")`
`colnames(subject_train)=c("Subject_Label")`
`colnames(y_test)=c("Y_Label")`
`colnames(subject_test)=c("Subject_Label")`
`colnames(x_test) <- (feature_labels[,3])`
`colnames(x_train) <- (feature_labels[,3])`


## Consolidating 

I'll use dplyr to join my `subject_train` with `y_train`, `activity_labels`, and `x_train` to form the `Training` data set. I'll do a very similar process to create the `Test` data set. I'll union them with `rbind` to form the consolidated training and test data.
I also threw in an indicator field "Dataset" to denote whether a row came from the Test or Training dataset.


`Training <- subject_train %>% `
 ` cbind(y_train) %>% `
  `left_join(activity_labels,by=c("Y_Label"="Index")) %>% `
  `mutate(Dataset = "Train") %>% `
  `cbind(x_train)`

`Test <- subject_test %>% `
  `cbind(y_test) %>% `
  `left_join(activity_labels,by=c("Y_Label"="Index")) %>% `
  `mutate(Dataset = "Test") %>% `
  `cbind(x_test)`

`Consolidated <- Training %>% `
  `rbind(Test)`
  
  
## Subsetting 

The assignment asked for only features that calculated mean or standard deviations, so I use the `grepl` function to identify columns within `Consolidated` that include either "mean()" or "std()".
From Consolidated, I can select only the target columns into `Trimmed`


`mean_cols_boolean <- grepl("mean()",fixed=TRUE,colnames(Consolidated))`
`sd_cols_boolean <- grepl("std()",fixed=TRUE,colnames(Consolidated))`
`mean_cols <- colnames(Consolidated[mean_cols_boolean])`
`sd_cols <- colnames(Consolidated[sd_cols_boolean])`

`Trimmed <- Consolidated %>% `
  `select(1:4,mean_cols,sd_cols)`


## Summarize

After I make sure there are no missing values in any of the columns I have selected, I am able to use dplyr to group by subject, activity, or both while calculating the mean of each mean() and std() column.


`Means_by_Subject <- Trimmed %>% `
  `group_by(Subject_Label) %>% `
  `summarise_all(funs(mean)) %>% `
  `select(Subject_Label,5:70)`


`Means_by_Activity <- Trimmed %>% `
  `group_by(activity) %>% `
  `summarise_all(funs(mean)) %>% `
  `select(activity,5:70)`


`Means_by_Subject_Activity <- Trimmed %>% `
  `group_by(Subject_Label,activity) %>% `
  `summarise_all(funs(mean)) %>% `
  `select(Subject_Label,activity,5:70)`
  

The activity prompt is slightly ambiguous on which grouped dataset we're supposed to be generating, so I'm interpreting it to want the dataset that is grouped by both subject and activity

`TidyDataset <- Means_by_Subject_Activity`


## Writing Table

Use `write.table(TidyDataset,"Tidy Dataset.txt",row.names = FALSE)` to create a .txt file of our tidy dataset to your working directory.