ReadMe
======

###This document contains instructions for working with run_analysis.R

Get the data set:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Extract into your working directory

Make sure run_analysis.R is in your working directory

Make sure to load  library(dplyr)

run run_analysis()

load the output "data_out.txt"<br> 
*read.table("data_out.txt", header = TRUE)*


run_analysis.R will:

* Merge the training and the test sets to create one data set.
* Extract only the measurements on the mean and standard deviation for each measurement.
* Output a  tidy data (one variable per column,all original measurements are from raw signals and
  each average in its own row ) text file  "data_out.txt"  
  with the average of each variable for each activity and each subject.

Specific description of the tidy data file contents in the codebook.md