# PANAS study TUE sample 2018 and 2019

These samples were collecting during intro psych course at TUE at 2018 and 2019. 

The zipped folder contain the raw downloaded data from surveymonkey. 
Because we used the same surveymonkey survey, we could just download all data at once after the replication study in 2019.
The students_0HV10_2018.xlsx is a list of all students in 2018.
The students_0HV10_2019.xlsx is a list of all students in 2019.
The Big_Five_0hv10_2018 contains student ID and age and gender info for students who filled in that questionnaire.
The results-survey229138_class_assignment_2019 file contains age and gender for students who did the in class partner SESOI task

# Making the data anonymous

The PANAS_1.csv and PANAS_2.csv files are CREATED from the latest zipfiles, based on the xlsx files (PANAS_1_2018_2019.xlsx and PANAS_2_2018_2019.xlsx) after copying the column names to the top row for PANAS items (removing the 2nd empty row), manually deleting the first columns up to that containing the student ID, which is renamed StudentID. Responses at the end renamed to Global_PA and Global_NA.
A Cohort column is created manuall from the date, seperating the 2018 and 2019 data collection.
This file is then saved. 
PANAS_2018_2019_anonimize.R creates 2 anonymous files by changing the student ID. 
Saved as PANAS_1_anon.csv and PANAS_2_anon.csv

# Merging the data
PANAS_2018_2019_merge.R merges the 2 files (so combines data for which 2 complete cases are reported, and where students filled out their student ID correctly at both instances. 
StudentID column is deleted. Even though it is anonymous, it is of no use and we also delete ID for prolific data.
Saves 316 complete cases as PANAS_TUE_2018_2019.csv

# PANAS study prolific sample

PANAS_1_prolific.csv contains the data from prolific. This data is already cleaned, using the script: PANAS_1_prolific_cleaning_split.R
This script is also used to randomly sample participants for the 2 day and 5 day conditions.
We did not use a seed, but the choice of assignment is indicated by the two created files splitting the sample in 2.
These 2 files are also inclued (prolificIDs2days.csv and prolificIDs5days.csv). 
The prolific data has 2 cohorts - one for 2 days between T1 and T2, one for 5 days. 

# Data merging

PANAS_prolific_merge.R takes the T1 data from file PANAS_1_prolific.csv and the T2 data from PANAS_2_prolific_2days.csv and PANAS_2_prolific_2days.csv. 
The Delay.Days column is renamed "Cohort" for later merging.
Merged data is saved as PANAS_prolific.csv.

# Combining data from TUE and prolific in 1 dataset. 

PANAS_combine_TUE_prolific_data.R combined both PANAS_TUE_2018_2019.csv and PANAS_prolific.csv into the analysis data: PANAS_total_analysis_data.csv
This datafile has 775 unique complete datapoints, and 43 columns. 
20 PANAS items at time 1 and 2, a Cohort column (2018 and 2019 for TUE data, 2 day and 5 day for prolific data) and the global PA and globalNA response. 
