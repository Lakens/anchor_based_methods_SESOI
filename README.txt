1) Prolific Study T1, wrangling takes Prolific Study T1.csv and removes people who did not complete, and deletes a duplicate case.

2) Prolific Study merging T1&T2.R takes the files Prolific Study T1.csv and Prolific Study T2 2days.csv and Prolific Study T2 5days.csv and merges these into a pre-post dataset which is stored as Prolific Study T1&T2.csv

3) Study 1&2 combining datasets.R takes Study 1 T1&T2.csv and Prolific Study T1&T2_anon.csv (the results from Study 1) and merges them and saves them as Studies 1&2, combined.csv. 

4) we can now run Studies 1&2, combined, analyses.R on Studies 1&2, combined.csv. This file contains our preregistered analysis. 

