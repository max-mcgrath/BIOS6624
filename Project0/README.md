# Reproducibility Information

The `Background` folder contains information pertinent to understanding the
analysis but unnecessary for reproducing it. The `Code` folder contains five
files: `1-DataPrep.R`, `2-EDA.R`, `3-Analysis-Aim1.R`, `4-Analysis-Aim2.R`, and
`5-Analysis-Aim3.R`. These R scripts are dependent upon a data file 
`Data/Project0_Clean_v2.csv` which is not available on GitHub, but may be 
requested by emailing `max.mcgrath@ucdenver.edu`. 

To run the complete analysis, each
script should be run in the order of the number preceding its filename. Note
that the working directory should be set to `Project0` prior to running the 
code files. The last directory, `Report`, contains the RMarkdown file `report.Rmd` which may
be used to generate a report on the data (note that `report.Rmd` runs all
code files and is also dependent on the aforementioned data file)