source("Code/1_prepData.R")

# Univariate analysis - First run intercept only model, then use drug use at
#   baseline as predictor
lmVLOADIntOnly <- lm(LOG_VLOAD_DIFF ~ 1, data = cleanData)
lmVLOAD <- lm(LOG_VLOAD_DIFF ~ DRUGS_0, data = cleanData)
summary(lmVLOAD)
AIC(lmVLOAD)
AIC(lmVLOADIntOnly)

lmLEU3NIntOnly <- lm(LEU3N_DIFF ~ 1, data = cleanData)
lmLEU3N <- lm(LEU3N_DIFF ~ DRUGS_0, data = cleanData)
summary(lmLEU3N)
AIC(lmLEU3N)
AIC(lmLEU3NIntOnly)

lmMENTIntOnly <- lm(MENT_DIFF ~ 1, data = cleanData)
lmMENT <- lm(MENT_DIFF ~ DRUGS_0, data = cleanData)
summary(lmMENT)
AIC(lmMENT)
AIC(lmMENTIntOnly)

lmPHYSIntOnly <- lm(PHYS_DIFF ~ 1, data = cleanData)
lmPHYS <- lm(PHYS_DIFF ~ DRUGS_0, data = cleanData)
summary(lmPHYS)
AIC(lmPHYS)
AIC(lmPHYSIntOnly)

# Multivariate analysis
lmVLOAD_FULL <- lm(LOG_VLOAD_DIFF ~ DRUGS_0 + EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                       SMOKE_0 + ADH_2, data = cleanData)
summary(lmVLOAD_FULL)
AIC(lmVLOAD_FULL)


lmLEU3N_FULL <- lm(LEU3N_DIFF ~ DRUGS_0 + EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                  SMOKE_0 + ADH_2, data = cleanData)
summary(lmLEU3N_FULL)
AIC(lmLEU3N_FULL)

lmMENT_FULL <- lm(MENT_DIFF ~ DRUGS_0 + EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                 SMOKE_0 + ADH_2, data = cleanData)
summary(lmMENT_FULL)
AIC(lmMENT_FULL)

lmPHYS_FULL <- lm(PHYS_DIFF ~ DRUGS_0 + EDUC_0 + AGE_0 + RACE_0 + BMI_0 + 
                 SMOKE_0 + ADH_2, data = cleanData)
summary(lmPHYS_FULL)
AIC(lmPHYS_FULL)
        