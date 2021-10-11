## Project 1

This project is a secondary data analysis of the Multicenter AIDS Cohort Study, 
an ongoing prospective cohort study of the natural and treated histories of 
HIV-1 infection in homosexual and bisexual men in 4 major cities in the United 
States. 

This analysis seeks to understand how treatment response 2 years after 
initiating Highly active antiretroviral treatment (HAART) differs between 
subjects who report using hard drugs, such as 
heroine, at baseline and other subjects, who did not report hard drug use at 
baseline. 

The Background folder contains information pertinent to understanding the 
analysis but unnecessary for reproducing it. The Code folder contains six 
files: 1_prepData.R, 2_EDA.R, 3_FrequentistAnalysis.R, 4_BA_DrawChains.R, 
5_BA_AssessChains.R and 6_BA_AnalyzeChains.R. These R scripts are dependent upon
a data file Data/hiv_6624.csv which is not available on GitHub, but may be 
requested by emailing max.mcgrath@ucdenver.edu. To run the complete analysis, 
each script should be run in the order of the number preceding its filename. The
last directory, Report, contains the RMarkdown file report.Rmd which may be used
to generate this report
(note that it also depends on the aforementioned data and scripts).