#### Citation: Code taken from Emily Cooper (and modified) #####
library(dplyr)

# Read in raw data
dta <- read.csv("DataRaw/frmgham2.csv") 

# Remove people who had a stroke prior to study entry
remove <- dta$RANDID[which(dta$PREVSTRK==1 & dta$PERIOD==1)] 
dta <- subset(dta, !(dta$RANDID %in% remove))

# Create a new time variable where times > 10 years are set as exactly 10 years
dta$timestrk10yr <- ifelse(dta$TIMESTRK>=3650, 3650, dta$TIMESTRK) 

# Create an indicator variable for event (0=censored, 1=stroke)
dta$stroke10yr <- ifelse(dta$timestrk10yr<3650 & dta$STROKE==1, 1, 0) 

# For subjects where time of death < time of stroke, use the smaller value for event time
dta$timestrk10yr <- pmin(dta$TIMEDTH, dta$timestrk10yr) 

# Create CVD variable
dta$CVD <- ifelse(dta$PREVAP==1 | dta$PREVCHD==1 | dta$PREVMI==1, 1, 0)

# Remove incomplete observations, limit observations to period 1
dta <- subset(dta, is.na(dta$BPMEDS)==F & is.na(dta$TOTCHOL)==F & 
                  is.na(dta$BMI)==F)
dta <- subset(dta, dta$PERIOD==1)

# Create quantiles for continuous predictors
dta <- dta %>%
    mutate(AGE_QUANT = cut(AGE, breaks = quantile(dta$AGE), 
                           labels = c("first", "second", "third", "fourth")),
           SYSBP_QUANT = cut(SYSBP, breaks = quantile(dta$SYSBP), 
                             labels = c("first", "second", "third", "fourth")),
           BMI_QUANT = cut(BMI, breaks = quantile(dta$BMI), 
                             labels = c("first", "second", "third", "fourth")),
           TOTCHOL_QUANT = cut(TOTCHOL, breaks = quantile(dta$TOTCHOL), 
                           labels = c("first", "second", "third", "fourth")))

# Create final clean data sets stratified by SEX
cleanDataM <- subset(dta, dta$SEX == 1)
cleanDataF <- subset(dta, dta$SEX == 2)

sum(cleanDataM$stroke10yr)
sum(cleanDataF$stroke10yr)
sum(cleanDataF$CVD)
