source("Code/1_prepData.R")

# Univariate analysis - First run intercept only model, then use drug use at
#   baseline as predictor
lmVLOAD <- lm(LOG_VLOAD_DIFF ~ DRUGS_0 + LOG_VLOAD_0, data = cleanData)
summary(lmVLOAD)

lmLEU3N <- lm(LEU3N_DIFF ~ DRUGS_0 + LEU3N_0, data = cleanData)
summary(lmLEU3N)

lmMENT <- lm(MENT_DIFF ~ DRUGS_0 + AGG_MENT_0, data = cleanData)
summary(lmMENT)

lmPHYS <- lm(PHYS_DIFF ~ DRUGS_0 + AGG_PHYS_0, data = cleanData)
summary(lmPHYS)

# Multivariate analysis
lmVLOAD_FULL <- lm(LOG_VLOAD_DIFF ~ DRUGS_0 + EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                       SMOKE_0 + ADH_2 + LOG_VLOAD_0, data = cleanData)
lmVLOAD_FULL_NO_DRUGS <- lm(LOG_VLOAD_DIFF ~ EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                       SMOKE_0 + ADH_2 + LOG_VLOAD_0, data = cleanData)
summary(lmVLOAD_FULL)
summary(lmVLOAD_FULL_NO_DRUGS)
AIC(lmVLOAD_FULL)


lmLEU3N_FULL <- lm(LEU3N_DIFF ~ DRUGS_0 + EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                  SMOKE_0 + ADH_2 + LEU3N_0, data = cleanData)
lmLEU3N_FULL_NO_DRUGS <- lm(LEU3N_DIFF ~ EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                       SMOKE_0 + ADH_2 + LEU3N_0, data = cleanData)
summary(lmLEU3N_FULL)
summary(lmLEU3N_FULL_NO_DRUGS)
AIC(lmLEU3N_FULL)

lmMENT_FULL <- lm(MENT_DIFF ~ DRUGS_0 + EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                 SMOKE_0 + ADH_2 + AGG_MENT_0, data = cleanData)
lmMENT_FULL_NO_DRUGS <- lm(MENT_DIFF ~ EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                      SMOKE_0 + ADH_2 + AGG_MENT_0, data = cleanData)
summary(lmMENT_FULL)
summary(lmMENT_FULL_NO_DRUGS)
AIC(lmMENT_FULL)

lmPHYS_FULL <- lm(PHYS_DIFF ~ DRUGS_0 + EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                 SMOKE_0 + ADH_2 + AGG_PHYS_0, data = cleanData)
lmPHYS_FULL_NO_DRUGS <- lm(PHYS_DIFF ~ EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                      SMOKE_0 + ADH_2 + AGG_PHYS_0, data = cleanData)
summary(lmPHYS_FULL)
summary(lmPHYS_FULL_NO_DRUGS)
AIC(lmPHYS_FULL)
        