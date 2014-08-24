# Assignment
# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names. 
# 5 Creates a second, independent tidy data set with the average of each variable for each activity and
#   each subject.

# metrics
# 1 Upload the tidy data set created in step 5 as a txt file created with write.table() using 
#   row.name=FALSE
# 2 Submit a link to a Github repo with the code for performing your analysis. The code should have 
#   a file run_analysis.R in the main directory that can be run as long as the Samsung data is in your
#   working directory. The output should be the tidy data set you submitted for part 1. You should 
#   include a README.md in the repo describing how the script works and the code book describing the 
#   variables.


# runs from a working directory with the following 6 data files from the UCI HAR Dataset:
# subject_train.txt and subject_test.txt
# X_train.txt and X_test.txt
# Y_train.txt and Y_test.txt

run_analysis <- function (){
    # build the dataset for steps 1-4 in the assingment, save as data-msd.txt
    #
    # each line of data has 561 numeric fields of 16 bytes in scientific notation
    # build the list of fields to read as only fields for mean or SD are wanted
    # see the codebook for a description of these fields
    mask<-c(
        rep(16,6),      # fields 1:6
        -rep(16,34),    # 7:40
        rep(16,6),      # 41:46
        -rep(16,34),    # 47:80
        rep(16,6),      # 81:86
        -rep(16,34),    # 87:120
        rep(16,6),      # 121:126
        -rep(16,34),    # 127:160
        rep(16,6),      # 161:166
        -rep(16,34),    # 167:200
        rep(16,2),      # 201:202
        -rep(16,11),    # 203:213
        rep(16,2),      # 214:215
        -rep(16,11),    # 216:226
        rep(16,2),      # 227:228
        -rep(16,11),    # 229:239
        rep(16,2),      # 240:241
        -rep(16,11),    # 242:252
        rep(16,2),      # 253:254
        -rep(16,11),    # 255:265
        rep(16,6),      # 266:271
        -rep(16,22),    # 272:293
        rep(16,3),      # 294:296
        -rep(16,48),    # 297:344
        rep(16,6),      # 345:350
        -rep(16,22),    # 351:372
        rep(16,3),      # 373:375
        -rep(16,48),    # 376:423
        rep(16,6),      # 424:429
        -rep(16,22),    # 430:451
        rep(16,3),      # 452:454
        -rep(16,48),    # 455:502
        rep(16,2),      # 503:504
        -rep(16,8),     # 505:512
        rep(16,1),      # 513
        -rep(16,2),     # 514:515
        rep(16,2),      # 516:517
        -rep(16,11),    # 518:528
        rep(16,2),      # 529:530
        -rep(16,8),     # 531:538
        rep(16,1),      # 539
        -rep(16,2),     # 540:541
        rep(16,2),      # 542:543
        -rep(16,8),     # 544:551
        rep(16,1),      # 552
        -rep(16,9))     # 553:561
    
    #process the test data
    #
    # read the data using read.fwf - this takes a while!
    data1<-read.fwf("./X_test.txt",mask, colClasses="numeric")
    data2<-read.fwf("./X_train.txt",mask, colClasses="numeric")
    # combine the data
    data3<-rbind(data,data2)
    # cleanup to save ram for next steps (may be necessary on smaller systems)
    remove(data1)
    remove(data2)
    # build the atype vector
    atype1<-read.table("./y_test.txt")
    atype2<-read.table("./y_train.txt")
    atype3<-rbind(atype1,atype2)
    # right cbind the atype
    data3<-cbind(atype3,data3)
    # build the subject vector
    subj1<-read.table("./subject_test.txt")
    subj2<-read.table("./subject_train.txt")
    subj3<-rbind(subj1,subj2)
    # right cbind the subject
    data3<-cbind(subj3,data3)
    # build the source vector
    src<-c(rep("test",length(subj1[,1])),rep("train",length(subj2[,1])))
    # right cbind them on
    data3<-cbind(src,data3)
    # add the column names
    cn<-c("set","subject","atype","tBodyAcc-mean-X","tBodyAcc-mean-Y","tBodyAcc-mean-Z","tBodyAcc-std-X","tBodyAcc-std-Y",
          "tBodyAcc-std-Z","tGravityAcc-mean-X","tGravityAcc-mean-Y","tGravityAcc-mean-Z","tGravityAcc-std-X","tGravityAcc-std-Y",
          "tGravityAcc-std-Z","tBodyAccJerk-mean-X","tBodyAccJerk-mean-Y","tBodyAccJerk-mean-Z","tBodyAccJerk-std-X",
          "tBodyAccJerk-std-Y","tBodyAccJerk-std-Z","tBodyGyro-mean-X","tBodyGyro-mean-Y","tBodyGyro-mean-Z","tBodyGyro-std-X",
          "tBodyGyro-std-Y","tBodyGyro-std-Z","tBodyGyroJerk-mean-X","tBodyGyroJerk-mean-Y","tBodyGyroJerk-mean-Z","tBodyGyroJerk-std-X",
          "tBodyGyroJerk-std-Y","tBodyGyroJerk-std-Z","tBodyAccMag-mean","tBodyAccMag-std","tGravityAccMag-mean","tGravityAccMag-std",
          "tBodyAccJerkMag-mean","tBodyAccJerkMag-std","tBodyGyroMag-mean","tBodyGyroMag-std","tBodyGyroJerkMag-mean","tBodyGyroJerkMag-std",
          "fBodyAcc-mean-X","fBodyAcc-mean-Y","fBodyAcc-mean-Z","fBodyAcc-std-X","fBodyAcc-std-Y","fBodyAcc-std-Z","fBodyAcc-meanFreq-X",
          "fBodyAcc-meanFreq-Y","fBodyAcc-meanFreq-Z","fBodyAccJerk-mean-X","fBodyAccJerk-mean-Y","fBodyAccJerk-mean-Z","fBodyAccJerk-std-X",
          "fBodyAccJerk-std-Y","fBodyAccJerk-std-Z","fBodyAccJerk-meanFreq-X","fBodyAccJerk-meanFreq-Y","fBodyAccJerk-meanFreq-Z",
          "fBodyGyro-mean-X","fBodyGyro-mean-Y","fBodyGyro-mean-Z","fBodyGyro-std-X","fBodyGyro-std-Y","fBodyGyro-std-Z","fBodyGyro-meanFreq-X",
          "fBodyGyro-meanFreq-Y","fBodyGyro-meanFreq-Z","fBodyAccMag-mean","fBodyAccMag-std","fBodyAccMag-meanFreq","fBodyBodyAccJerkMag-mean",
          "fBodyBodyAccJerkMag-std","fBodyBodyGyroMag-mean","fBodyBodyGyroMag-std","fBodyBodyGyroMag-meanFreq","fBodyBodyGyroJerkMag-mean",
          "fBodyBodyGyroJerkMag-std","fBodyBodyGyroJerkMag-meanFreq")
    names(data3)<-cn
    # write the set as data-msd.txt
    write.table(data,"./data-msd.txt",row.names=FALSE)
    #
    # done with the full dataset!
    #
    # delete the columns for the sd data
    rs<-c(1,2,3,4,5,7,11,12,13,17,18,19,23,24,25,29,30,31,35,37,39,41,43,45,46,47,51,
          52,53,54,55,56,60,61,62,63,64,65,69,70,71,72,74,75,77,79,80)
    data4<-data3[,rs]
    # write the set as data-m.txt
    write.table(data4,"./data-m.txt",row.names=FALSE)
}