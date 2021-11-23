source("Code/1_ProcessData.R")

# Load packages
library(survival)
library(survminer)

# Female sex -------------------------------------------------------------------
# Create Initial Cox Proportional Hazards Model
coxModelF_1 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD + TOTCHOL +
                         BMI, 
                     data = cleanDataF)
summary(coxModelF_1)

# Remove covariate with highest p-val (BMI), rebuild model
coxModelF_2 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD + TOTCHOL, 
                     data = cleanDataF)
summary(coxModelF_2)

# Remove covariate with highest p-val (TOTCHOL), rebuild model
coxModelF_3 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD, 
                     data = cleanDataF)
summary(coxModelF_3)

# Store final model
coxModelF <- coxModelF_3

# Male sex ---------------------------------------------------------------------
# Repeat process for subjects w/ male sex
# Create Initial Cox Proportional Hazards Model
coxModelM_1 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD + TOTCHOL +
                         BMI, 
                     data = cleanDataM)
summary(coxModelM_1)

# Remove covariate with highest p-val (BMI), rebuild model
coxModelM_2 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD + TOTCHOL, 
                     data = cleanDataM)
summary(coxModelM_2)

# Remove variable with highest p-val (TOTCHOL), rebuild model
coxModelM_3 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD, 
                     data = cleanDataM)
summary(coxModelM_3)

# Store final model
coxModelM <- coxModelM_3
