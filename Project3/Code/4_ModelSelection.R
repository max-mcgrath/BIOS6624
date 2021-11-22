source("Code/1_ProcessData.R")

# Load packages
library(survival)
library(survminer)

# Create Initial Cox Proportional Hazards Model
coxModelF_1 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD + TOTCHOL +
                         BMI, 
                     data = cleanDataF)
summary(coxModelF_1)

# Remove variable with highest p-val (BMI), rebuild model
coxModelF_2 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD + TOTCHOL, 
                     data = cleanDataF)
summary(coxModelF_2)

# Remove variable with highest p-val (TOTCHOL), rebuild model
coxModelF_3 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD, 
                     data = cleanDataF)
summary(coxModelF_3)

# Remove variable with highest p-val (BPMEDS), rebuild model
coxModelF_4 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + DIABETES + CURSMOKE + CVD, 
                     data = cleanDataF)
summary(coxModelF_4)

# Remove variable with highest p-val (DIABETES), rebuild model
coxModelF_5 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + CURSMOKE + CVD, 
                     data = cleanDataF)
summary(coxModelF_5)

# Remove variable with highest p-val (CVD), rebuild model
coxModelF_6 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + CURSMOKE, 
                     data = cleanDataF)
summary(coxModelF_6)

# Remove variable with highest p-val (CURSMOKE), rebuild model
coxModelF_7 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP, 
                     data = cleanDataF)
summary(coxModelF_7)

# All variables now significant, store final model
coxModelF <- coxModelF_7

# Repeat process for subjects w/ male sex
# Create Initial Cox Proportional Hazards Model
coxModelM_1 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + CVD + TOTCHOL +
                         BMI, 
                     data = cleanDataM)
summary(coxModelM_1)

# Remove variable with highest p-val (CVD), rebuild model
coxModelM_2 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + TOTCHOL + BMI, 
                     data = cleanDataM)
summary(coxModelM_2)

# Remove variable with highest p-val (BMI), rebuild model
coxModelM_3 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE + TOTCHOL, 
                     data = cleanDataM)
summary(coxModelM_3)

# Remove variable with highest p-val (TOTCHOL), rebuild model
coxModelM_4 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + BPMEDS + DIABETES + CURSMOKE, 
                     data = cleanDataM)
summary(coxModelM_4)

# Remove variable with highest p-val (TOTCHOL), rebuild model
coxModelM_5 <- coxph(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE +
                         SYSBP + DIABETES + CURSMOKE, 
                     data = cleanDataM)
summary(coxModelM_5)

# All variables significant at alpha = 0.5, store final model
coxModelM <- coxModelM_5
