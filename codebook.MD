---
title: "Codebook for Getting and Cleaning Data course project"
author: "WEWilliams"
date: "08/21/2014"
output: html_document
---

## Data Set Descriptions

The script creates 2 datasets, but only the smaller is uploaded 

### Data Fields in data-m.txt

The data-m dataset is created by extracting the first 3 descriptor fields and the mean values from the data-msd dataset described next.  
  
### Data Fields in data-msd.txt (not uploaded)  
  
The data set has both description and data fields, as shown below.  
  
Note that the names are the original names except the "()" have been removed to prevent any confusion with a function.  Mixed case and non-alphanumerics were retained because I find that "tBodyAcc-mean-X" is easier to read than "tbodyaccmeanx", and because it more closely matches the orginal.  
  
The description fields in each record are:  
field   Original #      name        description  
1       -               set         "test" or "train" dataset in the source  
2       -               subject     the test subject; based on subject_<set>.txt  
3       -               atype       shows the activity type; based on y_<set>.txt  
4       1               tBodyAcc-mean-X    
5       2               tBodyAcc-mean-Y  
7       3               tBodyAcc-mean-Z  
8       4               tBodyAcc-std-X  
9       5               tBodyAcc-std-Y  
10      6               tBodyAcc-std-Z  
11      41              tGravityAcc-mean-X  
12      42              tGravityAcc-mean-Y  
13      43              tGravityAcc-mean-Z  
14      44              tGravityAcc-std-X  
15      45              tGravityAcc-std-Y  
16      46              tGravityAcc-std-Z  
17      81              tBodyAccJerk-mean-X  
18      82              tBodyAccJerk-mean-Y  
19      83              tBodyAccJerk-mean-Z  
20      84              tBodyAccJerk-std-X  
21      85              tBodyAccJerk-std-Y  
22      86              tBodyAccJerk-std-Z  
23      121             tBodyGyro-mean-X  
24      122             tBodyGyro-mean-Y  
25      123             tBodyGyro-mean-Z  
26      124             tBodyGyro-std-X  
26      125             tBodyGyro-std-Y  
28      126             tBodyGyro-std-Z  
29      161             tBodyGyroJerk-mean-X  
30      162             tBodyGyroJerk-mean-Y  
31      163             tBodyGyroJerk-mean-Z  
32      164             tBodyGyroJerk-std-X  
33      165             tBodyGyroJerk-std-Y  
34      166             tBodyGyroJerk-std-Z  
35      201             tBodyAccMag-mean  
36      202             tBodyAccMag-std  
37      214             tGravityAccMag-mean  
38      215             tGravityAccMag-std  
39      227             tBodyAccJerkMag-mean  
40      228             tBodyAccJerkMag-std  
41      240             tBodyGyroMag-mean  
42      241             tBodyGyroMag-std  
43      253             tBodyGyroJerkMag-mean  
44      254             tBodyGyroJerkMag-std  
45      266             fBodyAcc-mean-X  
46      267             fBodyAcc-mean-Y  
47      268             fBodyAcc-mean-Z  
48      269             fBodyAcc-std-X  
49      270             fBodyAcc-std-Y  
50      271             fBodyAcc-std-Z  
51      294             fBodyAcc-meanFreq-X  
52      295             fBodyAcc-meanFreq-Y  
53      296             fBodyAcc-meanFreq-Z  
54      345             fBodyAccJerk-mean-X  
55      346             fBodyAccJerk-mean-Y  
56      347             fBodyAccJerk-mean-Z  
57      348             fBodyAccJerk-std-X  
58      349             fBodyAccJerk-std-Y  
59      350             fBodyAccJerk-std-Z  
60      373             fBodyAccJerk-meanFreq-X  
61      374             fBodyAccJerk-meanFreq-Y  
62      375             fBodyAccJerk-meanFreq-Z  
63      424             fBodyGyro-mean-X  
64      425             fBodyGyro-mean-Y  
65      426             fBodyGyro-mean-Z  
66      427             fBodyGyro-std-X  
67      428             fBodyGyro-std-Y  
68      429             fBodyGyro-std-Z  
69      452             fBodyGyro-meanFreq-X  
70      453             fBodyGyro-meanFreq-Y  
71      454             fBodyGyro-meanFreq-Z  
72      503             fBodyAccMag-mean  
73      504             fBodyAccMag-std  
74      513             fBodyAccMag-meanFreq  
75      516             fBodyBodyAccJerkMag-mean  
76      517             fBodyBodyAccJerkMag-std  
77      529             fBodyBodyGyroMag-mean  
78      530             fBodyBodyGyroMag-std  
79      539             fBodyBodyGyroMag-meanFreq  
80      542             fBodyBodyGyroJerkMag-mean  
81      543             fBodyBodyGyroJerkMag-std  
  

## Data Source
The data source was given on the Getting and Cleaning Data course project page, and was downloaded on 20140810.  

The source url given on that page is 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The orginal source was given as  
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
but was not confirmed.   

The user is urged to see the README.txt and other decsriptive files inlcuded with the source data to better understand the structure and content of the source data.  

The data follows Mcrosoft line end protocol (\r\n)  

Files used:  
x_test.txt  
- the actual data, with record structure described in features_info.txt and features.txt  
- each record is 16 bytes wide with 14 characters, or 15 if the value is negative  
y_test.txt - the activity type code value for each record 
subject_test.txt - the subject performing in each record  
For each 'test' file there is an equivalent 'train' file; both the 'test' and the 'train' files were used.  

## Analysis Environment & Tools
OS: Ubuntu 12.04  
R Version: 3.1.1  
RStudio: 0.98.953  
gedit 3.4.1 - a simple text editor to look at the data  
geex 12.0.3 - a hex editor to check the data's structure at a byte level  

## Raw Data  

### Data structure
the data is delivered in a zip file with the folling structure:  
unzipping creates the folder "UCI HAR DataSet" which contains:  
4 files: activity_labels.txt, features.txt, features_info.txt, README.txt  
folder test: with files subject_test.txt, X_test.txt, y_train.txt and a subfolder with futher data  
folder train: with files subject_train.txt, X_train.txt, y_train.txt and a subfolder with futher data   


### Initial Inspection
data was spot checked using gedit and jeex to look at: 
data files  
file content  
file structure  
Data appears to be well formed and consistant in structure  

