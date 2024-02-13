participant_data_T1.xlsx follows the preprocessing and CONN matrices order (Batch1-Batch2-Batch3-manual) and contains only the 628 subjects
--> When running GraphVar, make sure each subjects is assigned to the correct connectivity matrix

"The sample of 628 participants was further reduced to 613 due to missing cognitive data. Specifically, we excluded participants with more than 3 missing neuropsychological scores (N = 15). Then, we replaced any missing score of the remaining 613 participants with the appropriate median of their age decile."



------
CognitiveData.xlsx has additional subjects
--> When running data wrangling in R, make sure to subset only preprocessed subjects
--> Age of fMRI session and Age of cognitive data may not always be the same

