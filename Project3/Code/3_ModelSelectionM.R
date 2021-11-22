source("Code/1_ProcessData.R")

# Load packages
library(survival)
library(survminer)

# Create Cox Proportional Hazards Model
coxModelOne <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE + 
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD + TOTCHOL, 
                     data = cleanDataM)
summary(coxModelOne)


