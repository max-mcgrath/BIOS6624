source("Code/1_ProcessData.R")

#### Citation: some ggsurvplot args taken from Joe Froelicher ####

library(survival)
library(survminer)

kmPlotsM <- list()
kmPlotsF <- list()

# Estimate K-M survival curves w/ no covariates
kmF1 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ 1, 
                data = cleanDataF)
kmPlotsF[["INTERCEPT"]] <- ggsurvplot(kmF1, legend = "none", conf.int = FALSE,
                                      ylim = c(.95, 1),
                                      xlim = c(0, 3650),
                                      title = "No covariates",
                                      font.title=c(8,"bold","black"),
                                      font.tickslab = c(8),
                                      xlab = NULL,
                                      ylab = NULL,
                                      censor = FALSE)

kmM1 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ 1, 
                data = cleanDataM)
kmPlotsM[["INTERCEPT"]] <- ggsurvplot(kmM1, legend = "none", conf.int = FALSE, 
                            ylim = c(.95, 1),
                            xlim = c(0, 3650),
                            title = "No covariates",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE)

# Estimate K-M survival curves w/ AGE
kmF2 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE_QUANT, 
                data = cleanDataF)
kmPlotsF[["AGE"]] <- ggsurvplot(kmF2, legend = "right", conf.int = FALSE, 
                            ylim = c(.93, 1),
                            xlim = c(0, 3650),
                            title = "Age",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = paste0(quantile(cleanDataF$AGE)[1:4], 
                                                 "-",
                                                 quantile(cleanDataF$AGE)[2:5]),
                            legend.title = "Age")

kmM2 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ AGE_QUANT, 
                data = cleanDataM)
kmPlotsM[["AGE"]] <- ggsurvplot(kmM2, legend = "right", conf.int = FALSE, 
                            ylim = c(.94, 1),
                            xlim = c(0, 3650),
                            title = "Age",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = paste0(quantile(cleanDataM$AGE)[1:4], 
                                                 "-",
                                                 quantile(cleanDataM$AGE)[2:5]),
                            legend.title = "Age")

# Estimate K-M survival curves w/ SYSBP
kmF3 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ SYSBP_QUANT, 
                data = cleanDataF)
kmPlotsF[["SYSBP"]] <- ggsurvplot(kmF3, legend = "right", conf.int = FALSE, 
                            ylim = c(.92, 1),
                            xlim = c(0, 3650),
                            title = "Systolic Blood Pressure",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = paste0(quantile(cleanDataF$SYSBP)[1:4], 
                                                 "-",
                                                 quantile(cleanDataF$SYSBP)[2:5]),
                            legend.title = "mmHg")

kmM3 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ SYSBP_QUANT, 
                data = cleanDataM)
kmPlotsM[["SYSBP"]] <- ggsurvplot(kmM3, legend = "right", conf.int = FALSE, 
                            ylim = c(.92, 1),
                            xlim = c(0, 3650),
                            title = "Systolic Blood Pressure",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = paste0(quantile(cleanDataM$SYSBP)[1:4], 
                                                 "-",
                                                 quantile(cleanDataM$SYSBP)[2:5]),
                            legend.title = "mmHg")

# Estimate K-M survival curves w/ BMI
kmF4 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ BMI_QUANT, 
                data = cleanDataF)
kmPlotsF[["BMI"]] <- ggsurvplot(kmF4, legend = "right", conf.int = FALSE, 
                            ylim = c(.95, 1),
                            xlim = c(0, 3650),
                            title = "Body Mass Index",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = paste0(round(quantile(cleanDataF$BMI)[1:4], 0), 
                                                 "-",
                                                 round(quantile(cleanDataF$BMI)[2:5], 0)),
                            legend.title = "BMI")

kmM4 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ BMI_QUANT, 
                data = cleanDataM)
kmPlotsM[["BMI"]] <- ggsurvplot(kmM4, legend = "right", conf.int = FALSE, 
                            ylim = c(.96, 1),
                            xlim = c(0, 3650),
                            title = "Body Mass Index",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = paste0(round(quantile(cleanDataM$BMI)[1:4], 0),
                                                 "-",
                                                 round(quantile(cleanDataM$BMI)[2:5], 0)),
                            legend.title = "BMI")

# Estimate K-M survival curves w/ TOTCHOL
kmF5 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ TOTCHOL_QUANT, 
                data = cleanDataF)
kmPlotsF[["TOTCHOL"]] <- ggsurvplot(kmF5, legend = "right", conf.int = FALSE, 
                            ylim = c(.96, 1),
                            xlim = c(0, 3650),
                            title = "Total Cholesterol (mg/Dl)",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = paste0(round(quantile(cleanDataF$TOTCHOL)[1:4], 0), 
                                                 "-",
                                                 round(quantile(cleanDataF$TOTCHOL)[2:5], 0)),
                            legend.title = "mg/Dl")

kmM5 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ TOTCHOL_QUANT, 
                data = cleanDataM)
kmPlotsM[["TOTCHOL"]] <- ggsurvplot(kmM5, legend = "right", conf.int = FALSE, 
                            ylim = c(.96, 1),
                            xlim = c(0, 3650),
                            title = "Total Cholesterol (mg/Dl)",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = paste0(round(quantile(cleanDataM$TOTCHOL)[1:4], 0), 
                                                 "-",
                                                 round(quantile(cleanDataM$TOTCHOL)[2:5], 0)),
                            legend.title = "mg/Dl")

# Estimate K-M survival curves w/ BPMEDS
kmF6 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ BPMEDS, 
                data = cleanDataF)
kmPlotsF[["BPMEDS"]] <- ggsurvplot(kmF6, legend = "right", conf.int = FALSE, 
                            ylim = c(.85, 1),
                            xlim = c(0, 3650),
                            title = "Anti-hypertension Treatment",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = c("No", "Yes"),
                            legend.title = "Using treatment")

kmM6 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ BPMEDS, 
                data = cleanDataM)
kmPlotsM[["BPMEDS"]] <- ggsurvplot(kmM6, legend = "right", conf.int = FALSE, 
                            ylim = c(.8, 1), 
                            xlim = c(0, 3650),
                            title = "Anti-hypertension Treatment",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = c("No", "Yes"),
                            legend.title = "Using treatment")

# Estimate K-M survival curves w/ CURSMOKER
kmF7 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ CURSMOKE, 
                data = cleanDataF)
kmPlotsF[["CURSMOKER"]] <- ggsurvplot(kmF7, legend = "right", conf.int = FALSE, 
                            ylim = c(.95, 1),
                            xlim = c(0, 3650),
                            title = "Current Smoker",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = c("No", "Yes"),
                            legend.title = "Smoker")

kmM7 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ CURSMOKE, 
                data = cleanDataM)
kmPlotsM[["CURSMOKER"]] <- ggsurvplot(kmM7, legend = "right", conf.int = FALSE, 
                            ylim = c(.95, 1),
                            xlim = c(0, 3650),
                            title = "Current Smoker",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = c("No", "Yes"),
                            legend.title = "Smoker")

# Estimate K-M survival curves w/ DIABETES
kmF8 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ DIABETES, 
                data = cleanDataF)
kmPlotsF[["DIABETES"]] <- ggsurvplot(kmF8, legend = "right", conf.int = FALSE, 
                            ylim = c(.90, 1),
                            xlim = c(0, 3650),
                            title = "Diabetic",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = c("No", "Yes"),
                            legend.title = "Diabetic")

kmM8 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ DIABETES, 
                data = cleanDataM)
kmPlotsM[["DIABETES"]] <- ggsurvplot(kmM8, legend = "right", conf.int = FALSE, 
                            ylim = c(.8, 1),
                            xlim = c(0, 3650),
                            title = "Diabetic",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = c("No", "Yes"),
                            legend.title = "Diabetic")

# Estimate K-M survival curves w/ CVD
kmF9 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ CVD, 
                data = cleanDataF)
kmPlotsF[["CVD"]] <- ggsurvplot(kmF9, legend = "right", conf.int = FALSE, 
                            ylim = c(.8, 1),
                            xlim = c(0, 3650),
                            title = "History of CVD",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = c("No", "Yes"),
                            legend.title = "CVD")

kmM9 <- survfit(Surv(time = timestrk10yr, event = stroke10yr) ~ CVD, 
                data = cleanDataM)
kmPlotsM[["CVD"]] <- ggsurvplot(kmM9, legend = "right", conf.int = FALSE, 
                            ylim = c(.90, 1),
                            xlim = c(0, 3650),
                            title = "History of CVD",
                            font.title=c(8,"bold","black"),
                            font.tickslab = c(8),
                            xlab = NULL,
                            ylab = NULL,
                            censor = FALSE,
                            legend.labs = c("No", "Yes"),
                            legend.title = "CVD")
