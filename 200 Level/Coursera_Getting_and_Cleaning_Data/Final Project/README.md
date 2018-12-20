# Coursera Cleaning and Data Final Project
#### Author: Connor Voglewede
#### December 17, 2018







## Objective

My script run_analysis.R is designed to download, and clean, and aggregate a subset of data from the UCI *Human Activity Recognition data set*. 

Ultimately, the assignment is to clean the data and aggregate means of means and standard deviations for a subject of variables, grouped by unique subject-activity combination.

## Raw Data

Full details about the Human Activity Recognition Using Smartphones data set is available here:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

## run_analysis.R

run_analysis.R is my R script that downloads the raw data from the UCI website into a temporary file, cleans the dataset, and creates a tidy dataset "Tidy Dataset.txt"

There are six steps to this script, as outlined in detail in the Codebook.md file in this repo:

1. Importing the raw data
2. Organizing the raw data
3. Merging the disparate data sets
4. Subsetting the consolidated data set
5. Summarizing the required fields using dplyr
6. Writing the tidy data set to a .txt file




