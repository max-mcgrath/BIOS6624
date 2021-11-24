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
