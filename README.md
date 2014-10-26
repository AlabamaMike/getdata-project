# Getting and Cleaning Data Course Project
## Actions
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Scripts
The run_analysis.R script runs all the steps necessary to create the two datasets described in the project requirements.  This script requires the plyr library to be installed.  The script downloads the dataset to a local directory when it is run, and doesn't require the user to have downloaded the data prior to the analysis.

