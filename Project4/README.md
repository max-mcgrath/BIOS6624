## Project 4 - Simulation

This study aims to investigate the efficacy of several model selection 
techniques for linear regression across four case scenarios. More specifically,
it aims to evaluate the ability of a set of backwards selection and
regularization techniques to select models that retain significant predictors
and remove insignificant predictors across scenarios with varying sample sizes
and correlation structures between predictors. To do so, simulation was
used to create regression data sets with known underlying models, then each 
model selection technique was applied to each data set to build a model. 
Each model selection technique's performance was then be evaluated using
a number of criteria that quantify the technique's overall ability to identify 
significant predictors and to accurately estimate those predictors' known 
associations with the response.

The `Background` folder contains information pertinent to understanding the
analysis but unnecessary for reproducing it. The `Code` folder contains thirteen
files: `1_Case1a.R`, `2_Case1b.R`, `3_Case2a.R`, `4_Case2b.R`,
`5_SimAnalysis1a.R`, `6_MakeTables1a.R`,
`7_SimAnalysis1b.R`, `8_MakeTables1b.R`,
`9_SimAnalysis2a.R`, `10_MakeTables2a.R`,
`11_SimAnalysis2b.R`, `12_MakeTables2b.R`, and `13_MakePlots.R`. 
To run the complete analysis, each
script should be run in the order of the number prefixing its filename. The
last directory, `Report`, contains the RMarkdown file `report.Rmd` which may
be used to generate this report (note that it also depends on the aforementioned
scripts).