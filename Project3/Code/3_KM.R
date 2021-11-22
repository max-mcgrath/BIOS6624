source("Code/1_ProcessData.R")

# Estimate K-M survival curves w/ no covariates
kmF1 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ 1, 
                data = cleanDataF)
ggsurvplot(kmF1, legend = "non", conf.int = FALSE, ylim = c(.95, 1))

kmM1 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ 1, 
                data = cleanDataM)
ggsurvplot(kmM1, legend = "none", conf.int = FALSE, ylim = c(.95, 1))

# Estimate K-M survival curves w/ AGE
kmF2 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE_QUANT, 
                data = cleanDataF)
ggsurvplot(kmF2, legend = "right", conf.int = FALSE, ylim = c(.9, 1))

kmM2 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE_QUANT, 
                data = cleanDataM)
ggsurvplot(kmM2, legend = "right", conf.int = FALSE, ylim = c(.90, 1))

# Estimate K-M survival curves w/ SYSBP
kmF3 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ SYSBP_QUANT, 
                data = cleanDataF)
ggsurvplot(kmF3, legend = "right", conf.int = FALSE, ylim = c(.9, 1))

kmM3 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ SYSBP_QUANT, 
                data = cleanDataM)
ggsurvplot(kmM3, legend = "right", conf.int = FALSE, ylim = c(.90, 1))

# Estimate K-M survival curves w/ BMI
kmF4 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ BMI_QUANT, 
                data = cleanDataF)
ggsurvplot(kmF4, legend = "right", conf.int = FALSE, ylim = c(.9, 1))

kmM4 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ BMI_QUANT, 
                data = cleanDataM)
ggsurvplot(kmM4, legend = "right", conf.int = FALSE, ylim = c(.90, 1))

# Estimate K-M survival curves w/ TOTCHOL
kmF5 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ TOTCHOL_QUANT, 
                data = cleanDataF)
ggsurvplot(kmF5, legend = "right", conf.int = FALSE, ylim = c(.9, 1))

kmM5 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ TOTCHOL_QUANT, 
                data = cleanDataM)
ggsurvplot(kmM5, legend = "right", conf.int = FALSE, ylim = c(.90, 1))

# Estimate K-M survival curves w/ BPMEDS
kmF6 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ BPMEDS, 
                data = cleanDataF)
ggsurvplot(kmF6, legend = "right", conf.int = FALSE, ylim = c(.8, 1))

kmM6 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ BPMEDS, 
                data = cleanDataM)
ggsurvplot(kmM6, legend = "right", conf.int = FALSE, ylim = c(.8, 1))

# Estimate K-M survival curves w/ CURSMOKER
kmF7 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ CURSMOKE, 
                data = cleanDataF)
ggsurvplot(kmF7, legend = "right", conf.int = FALSE, ylim = c(.8, 1))

kmM7 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ CURSMOKE, 
                data = cleanDataM)
ggsurvplot(kmM7, legend = "right", conf.int = FALSE, ylim = c(.8, 1))

# Estimate K-M survival curves w/ DIABETES
kmF8 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ DIABETES, 
                data = cleanDataF)
ggsurvplot(kmF8, legend = "right", conf.int = FALSE, ylim = c(.8, 1))

kmM8 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ DIABETES, 
                data = cleanDataM)
ggsurvplot(kmM8, legend = "right", conf.int = FALSE, ylim = c(.8, 1))

# Estimate K-M survival curves w/ CVD
kmF9 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ CVD, 
                data = cleanDataF)
ggsurvplot(kmF9, legend = "right", conf.int = FALSE, ylim = c(.8, 1))

kmM9 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ CVD, 
                data = cleanDataM)
ggsurvplot(kmM9, legend = "right", conf.int = FALSE, ylim = c(.8, 1))
