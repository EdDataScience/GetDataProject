---
title: "Readme"
author: "WEWilliams"
date: "08/23/2014"
output: html_document
---
# run_analysis.R  
  
##Assignment  

1 Merges the training and the test sets to create one data set.  
2 Extracts only the measurements on the mean and standard deviation for each measurement.   
3 Uses descriptive activity names to name the activities in the data set  
4 Appropriately labels the data set with descriptive variable names.   
5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

##Code Structure  
  
The script performs the following actions:  
1. Creates a mask to cause read.table to reead only the mean and standard deviation values  
2. Reads both datasets into data frames.  
3. Combinds the data frames to create the bulk data.  
4. Deletes un-needed data frames to conseve memory; this is likely not needed, but may prevent issues on some systems.  
5. Adds the activity type descriptor by reading in the y_???.txt data from both file sets, combining them, and adding the result to the left of the data frame.  
6. Adds the subject identifyer by reading i the subject_???.txt data from both files set, combining them, and adding the result to the left of the data frame.   
7. Adds the source description by creating a list of the words "test" and "train" that matches the lengths of each dataset, then adding the result to the left of the data frame.  
8.  Adds the column headers to replace the default names.  
9. writes the first dataset to the file "data-msd.txt"  
10. extracts the columns needed for the second output dataset into a new data frame  
11. writes the second dataset to the file "data-m.txt".  

##Environment

###Input Files  

The script will run in a directory with the following 6 data files from the UCI HAR Dataset:  
-    subject_train.txt and subject_test.txt  
-    X_train.txt and X_test.txt  
-    Y_train.txt and Y_test.txt  

###Output Files

The script writes 2 dataset:  
-  data-msd.txt  -  identifying data and the mean and standard deviations from the input files  
-  data-m.ext    -  identifying data andthe meas from the input files; only this dataset is uploaded  
  