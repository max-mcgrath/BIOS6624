## Project 3

This project is analysis of the Framingham Study cohort that aims to identify
statistically significant risk factors associated with greater risk of stroke
after 10 years among the studied cohort. Additionally, the study aims to 
determine 10-year probabilities of strokes for different risk profiles using 
the significant risk factors and to identify whether their is meaningful change 
in risk factors over the first 10 years of the study to determine whether a 
longitudinal analysis approach is warranted. This study is stratified by sex, 
meaning males and females are treated as two separate populations with 
potentially differing risk factors.

The `Background` folder contains information pertinent to understanding the
analysis but unnecessary for reproducing it. The `Code` folder contains six
files: `1_ProcessData.R`, `2_EDA.R`, `3_KM.R`, 
`4_ModelSelection.R`, `5_ProbabilitiesM.R`, `6_ProbabilitiesF.R`, and 
`7_TimeAnalysis.R`.
These R scripts are dependent upon a data file 
`Data/frmgham2.csv` which is not available on GitHub, but may be 
requested
by emailing `max.mcgrath@ucdenver.edu`. To run the complete analysis, each
script should be run in the order of the number prefixing its filename. The
last directory, `Report`, contains the RMarkdown file `report.Rmd` which may
be used to generate this report (note that it also depends on the aforementioned
data and scripts).
