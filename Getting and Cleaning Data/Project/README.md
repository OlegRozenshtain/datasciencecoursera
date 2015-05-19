The whole analysis is made in one script named **run_analysis.R**

### Analysis steps:
1. read all all data and lables tables from original study data.
2. attach in front of each feaute vector its corresponding subject id and activity type.
3. merge testing and training sets into one data set.
4. get the column indexies of the statistical measurements required (means and standard deviations).
5. subset the full data set and keep only the subject id, activity type and the desired columns from 4.
6. rename activity type from a number code (1-6) to the activity name given by activity_labels.txt in the original data.
7. give meaninful names to the columns.
8. calculate the average of each statistical measurement for each activity and each subject.
9. output results to a txt file.