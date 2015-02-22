
#Study Design

The data is dowloaded to a 'Data' directory from [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).  The files containg the data are:

- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.
- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- 'train/subject_test.txt': Same as previous

The raw data is loaded into data sets for transformations.  For each record, the following is provided:
 
- A 561-feature vector with time and frequency domain variables (x_test and x_train).
- Its activity label (y_test and y_train).
- An identifier of the subject who carried out the experiment (subject_test and subject_train).

The data is combined for test and training results, the variables of the data set are given descriptive names, and the activities are given descriptive activity names.  The resulting combined data set is 10299 observations of 81 variables.

The tidy data set contains only the measurements on the mean and standard deviation for each measurement.  The tidy data set is then aggregated with the average of each variable for each activity and each subject.  The resulting tidy data contains 180 observations (30 subjects X 6 activities) of 81 variables (79 variables containing either 'mean' or 'std' plus 'Subject' and 'Activity')

#Code Book

    varName 	  varType 	varRange
1 	Subject 	  integer 	  1 to 30
2 	Activity 	  character LAYING, SITTING, STANDING, 
                          WALKING, WALKING_DOWNSTAIRS, 
                          WALKING_UPSTAIRS
3 	tBodyAcc_mean_X 	numeric 	0.2216 to 0.30146
4 	tBodyAcc_mean_Y 	numeric 	-0.04051 to -0.00131
5 	tBodyAcc_mean_Z 	numeric 	-0.15251 to -0.07538
6 	tGravityAcc_mean_X 	numeric 	-0.68004 to 0.97451
7 	tGravityAcc_mean_Y 	numeric 	-0.47989 to 0.95659
8 	tGravityAcc_mean_Z 	numeric 	-0.49509 to 0.95787
9 	tBodyAccJerk_mean_X 	numeric 	0.04269 to 0.13019
10 	tBodyAccJerk_mean_Y 	numeric 	-0.03869 to 0.05682
11 	tBodyAccJerk_mean_Z 	numeric 	-0.06746 to 0.03805
12 	tBodyGyro_mean_X 	numeric 	-0.20578 to 0.1927
13 	tBodyGyro_mean_Y 	numeric 	-0.20421 to 0.02747
14 	tBodyGyro_mean_Z 	numeric 	-0.07245 to 0.1791
15 	tBodyGyroJerk_mean_X 	numeric 	-0.15721 to -0.02209
16 	tBodyGyroJerk_mean_Y 	numeric 	-0.07681 to -0.0132
17 	tBodyGyroJerk_mean_Z 	numeric 	-0.0925 to -0.00694
18 	tBodyAccMag_mean 	numeric 	-0.98649 to 0.6446
19 	tGravityAccMag_mean 	numeric 	-0.98649 to 0.6446
20 	tBodyAccJerkMag_mean 	numeric 	-0.99281 to 0.43449
21 	tBodyGyroMag_mean 	numeric 	-0.98074 to 0.418
22 	tBodyGyroJerkMag_mean 	numeric 	-0.99732 to 0.08758
23 	fBodyAcc_mean_X 	numeric 	-0.99525 to 0.53701
24 	fBodyAcc_mean_Y 	numeric 	-0.98903 to 0.52419
25 	fBodyAcc_mean_Z 	numeric 	-0.98947 to 0.28074
26 	fBodyAcc_meanFreq_X 	numeric 	-0.63591 to 0.15912
27 	fBodyAcc_meanFreq_Y 	numeric 	-0.37952 to 0.46653
28 	fBodyAcc_meanFreq_Z 	numeric 	-0.52011 to 0.40253
29 	fBodyAccJerk_mean_X 	numeric 	-0.99463 to 0.47432
30 	fBodyAccJerk_mean_Y 	numeric 	-0.9894 to 0.27672
31 	fBodyAccJerk_mean_Z 	numeric 	-0.99202 to 0.15778
32 	fBodyAccJerk_meanFreq_X 	numeric 	-0.57604 to 0.33145
33 	fBodyAccJerk_meanFreq_Y 	numeric 	-0.60197 to 0.19568
34 	fBodyAccJerk_meanFreq_Z 	numeric 	-0.62756 to 0.23011
35 	fBodyGyro_mean_X 	numeric 	-0.99312 to 0.47496
36 	fBodyGyro_mean_Y 	numeric 	-0.99403 to 0.32882
37 	fBodyGyro_mean_Z 	numeric 	-0.98596 to 0.49241
38 	fBodyGyro_meanFreq_X 	numeric 	-0.39577 to 0.24921
39 	fBodyGyro_meanFreq_Y 	numeric 	-0.66681 to 0.27314
40 	fBodyGyro_meanFreq_Z 	numeric 	-0.50749 to 0.37707
41 	fBodyAccMag_mean 	numeric 	-0.9868 to 0.58664
42 	fBodyAccMag_meanFreq 	numeric 	-0.31234 to 0.43585
43 	fBodyBodyAccJerkMag_mean 	numeric 	-0.994 to 0.5384
44 	fBodyBodyAccJerkMag_meanFreq 	numeric 	-0.12521 to 0.48809
45 	fBodyBodyGyroMag_mean 	numeric 	-0.98654 to 0.20398
46 	fBodyBodyGyroMag_meanFreq 	numeric 	-0.45664 to 0.40952
47 	fBodyBodyGyroJerkMag_mean 	numeric 	-0.99762 to 0.14662
48 	fBodyBodyGyroJerkMag_meanFreq 	numeric 	-0.18292 to 0.4263
49 	tBodyAcc_std_X 	numeric 	-0.99607 to 0.62692
50 	tBodyAcc_std_Y 	numeric 	-0.99024 to 0.61694
51 	tBodyAcc_std_Z 	numeric 	-0.98766 to 0.60902
52 	tGravityAcc_std_X 	numeric 	-0.99676 to -0.82955
53 	tGravityAcc_std_Y 	numeric 	-0.99425 to -0.64358
54 	tGravityAcc_std_Z 	numeric 	-0.99096 to -0.61016
55 	tBodyAccJerk_std_X 	numeric 	-0.9946 to 0.54427
56 	tBodyAccJerk_std_Y 	numeric 	-0.98951 to 0.35531
57 	tBodyAccJerk_std_Z 	numeric 	-0.99329 to 0.03102
58 	tBodyGyro_std_X 	numeric 	-0.99428 to 0.26766
59 	tBodyGyro_std_Y 	numeric 	-0.99421 to 0.47652
60 	tBodyGyro_std_Z 	numeric 	-0.98554 to 0.56488
61 	tBodyGyroJerk_std_X 	numeric 	-0.99654 to 0.17915
62 	tBodyGyroJerk_std_Y 	numeric 	-0.99708 to 0.29595
63 	tBodyGyroJerk_std_Z 	numeric 	-0.99538 to 0.19321
64 	tBodyAccMag_std 	numeric 	-0.98646 to 0.42841
65 	tGravityAccMag_std 	numeric 	-0.98646 to 0.42841
66 	tBodyAccJerkMag_std 	numeric 	-0.99465 to 0.45061
67 	tBodyGyroMag_std 	numeric 	-0.98137 to 0.29998
68 	tBodyGyroJerkMag_std 	numeric 	-0.99767 to 0.25017
69 	fBodyAcc_std_X 	numeric 	-0.9966 to 0.65851
70 	fBodyAcc_std_Y 	numeric 	-0.99068 to 0.56019
71 	fBodyAcc_std_Z 	numeric 	-0.98722 to 0.68712
72 	fBodyAccJerk_std_X 	numeric 	-0.99507 to 0.4768
73 	fBodyAccJerk_std_Y 	numeric 	-0.99047 to 0.34977
74 	fBodyAccJerk_std_Z 	numeric 	-0.99311 to -0.00624
75 	fBodyGyro_std_X 	numeric 	-0.99465 to 0.19661
76 	fBodyGyro_std_Y 	numeric 	-0.99435 to 0.64623
77 	fBodyGyro_std_Z 	numeric 	-0.98673 to 0.52245
78 	fBodyAccMag_std 	numeric 	-0.98765 to 0.17868
79 	fBodyBodyAccJerkMag_std 	numeric 	-0.99437 to 0.31635
80 	fBodyBodyGyroMag_std 	numeric 	-0.98147 to 0.23666
81 	fBodyBodyGyroJerkMag_std 	numeric 	-0.99759 to 0.28783