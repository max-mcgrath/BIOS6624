source("Code/4_ModelSelection.R")

# Calculate average covariate values
sysbpMean <- mean(cleanDataM$SYSBP)
bpmedsMean <- mean(cleanDataM$BPMEDS)
cursmokeMean <- mean(cleanDataM$CURSMOKE)
cholMean <- mean(cleanDataM$TOTCHOL)
bmiMean <- mean(cleanDataM$BMI)
diabetesMean <- mean(cleanDataM$DIABETES)
cvdMean <- mean(cleanDataM$DIABETES)

# Calculate continuous covariate means
sysbpQuants <- quantile(cleanDataM$SYSBP)
diabetesQuants <- quantile(cleanDataM$DIABETES)
bmiQuants <- quantile(cleanDataM$BMI)
totcholQuants <- quantile(cleanDataM$TOTCHOL)
cvdMean <- mean(cleanDataM$CVD)

# Create data frame to store survival probabilities
survProbDFM <- data.frame(
    age = c(55, 60, 65, 70, 75, 80, 85),
    average = rep(NA, 7),
    cursmokeYes = rep(NA, 7),
    bpmedsYes = rep(NA, 7),
    bpmedsNo = rep(NA, 7),
    diabetesYes = rep(NA, 7),
    cvdYes = rep(NA, 7),
    smokeYesDiabYes = rep(NA, 7),
    smokeYesCVDYes = rep(NA, 7),
    smokeYesBpmedsYes = rep(NA, 7),
    smokeYesBpmedsNo = rep(NA, 7),
    allConditions = rep(NA, 7)
)

survProbPlotsM <- list()

# All averages
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                          SYSBP = rep(sysbpMean, 7),
                          CURSMOKE = rep(cursmokeMean, 7),
                          DIABETES = rep(diabetesMean, 7),
                          BPMEDS = rep(bpmedsMean, 7),
                          CVD = rep(cvdMean, 7))
survProbPlotsM[[1]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$average <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]

# Current Smoker
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = rep(cvdMean, 7))
survProbPlotsM[[2]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$cursmokeYes <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]

# BPMEDS Yes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = rep(cursmokeMean, 7),
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = 1,
                      CVD = rep(cvdMean, 7))
survProbPlotsM[[3]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$bpmedsYes <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]

# BPMEDS No
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = rep(cursmokeMean, 7),
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = 0,
                      CVD = rep(cvdMean, 7))
survProbPlotsM[[4]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$bpmedsNo <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]

# CVD Yes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = rep(cursmokeMean, 7),
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = 1)
survProbPlotsM[[5]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$cvdYes <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]

# DIABETES Yes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = rep(cursmokeMean, 7),
                      DIABETES = 1,
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = rep(cvdMean, 7))
survProbPlotsM[[6]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$diabetesYes <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]

# Smoking and Diabetes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = 1,
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = rep(cvdMean, 7))
survProbPlotsM[[7]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$smokeYesDiabYes <- tail(survfit(coxModelM, 
                                            newdata = newData)$surv)[5, ]

# Smoking and CVD
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = 1)
survProbPlotsM[[8]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$smokeYesCVDYes <- tail(survfit(coxModelM, 
                                           newdata = newData)$surv)[5, ]

# Smoking and bpmeds yes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = 1,
                      CVD = rep(cvdMean, 7))
survProbPlotsM[[9]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$smokeYesBpmedsYes <- tail(survfit(coxModelM, 
                                              newdata = newData)$surv)[5, ]

# Smoking and bpmeds no
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = 0,
                      CVD = rep(cvdMean, 7))
survProbPlotsM[[10]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                  data = cleanDataM, censor = FALSE,
                                  conf.int = FALSE)
survProbDFM$smokeYesBpmedsNo <- tail(survfit(coxModelM, 
                                              newdata = newData)$surv)[5, ]

# All conditions
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = 1,
                      BPMEDS = 1,
                      CVD = 1)
survProbPlotsM[[11]] <- ggsurvplot(survfit(coxModelM, newdata = newData),
                                   data = cleanDataM, censor = FALSE,
                                   conf.int = FALSE)
survProbDFM$allConditions <- tail(survfit(coxModelM, 
                                             newdata = newData)$surv)[5, ]


# # Old code w/ sysbp stratified by quantiles, may be of interest later
# # SYSBP at each quantile
# # Quantile 1
# newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                           SYSBP = rep(sysbpQuants[1], 7),
#                           CURSMOKE = rep(cursmokeMean, 7),
#                           DIABETES = rep(diabetesMean, 7))
# ggsurvplot(survfit(coxModelM, newdata = newData), data = cleanDataM, 
#            censor = FALSE, conf.int = TRUE)
# survProbDFM$sysbpQuant1 <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]
# 
# # Quantile 2
# newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                       SYSBP = rep(sysbpQuants[2], 7),
#                       CURSMOKE = rep(cursmokeMean, 7),
#                       DIABETES = rep(diabetesMean, 7))
# ggsurvplot(survfit(coxModelM, newdata = newData), data = cleanDataM, 
#            censor = FALSE, conf.int = TRUE)
# survProbDFM$sysbpQuant2 <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]
# 
# # Quantile 3
# newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                       SYSBP = rep(sysbpQuants[3], 7),
#                       CURSMOKE = rep(cursmokeMean, 7),
#                       DIABETES = rep(diabetesMean, 7))
# ggsurvplot(survfit(coxModelM, newdata = newData), data = cleanDataM, 
#            censor = FALSE, conf.int = TRUE)
# survProbDFM$sysbpQuant3 <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]
# 
# # Quantile 4
# newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                       SYSBP = rep(sysbpQuants[4], 7),
#                       CURSMOKE = rep(cursmokeMean, 7),
#                       DIABETES = rep(diabetesMean, 7))
# ggsurvplot(survfit(coxModelM, newdata = newData), data = cleanDataM, 
#            censor = FALSE, conf.int = TRUE)
# survProbDFM$sysbpQuant4 <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]
# 
# # DIABETES no/yes
# # No
# newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                           SYSBP = rep(sysbpMean, 7),
#                           CURSMOKE = rep(cursmokeMean, 7),
#                           DIABETES = 0)
# ggsurvplot(survfit(coxModelM, newdata = newData), data = cleanDataM, 
#            censor = FALSE, conf.int = TRUE)
# survProbDFM$diabetesNo <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]
# 
# # Yes
# newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                       SYSBP = rep(sysbpMean, 7),
#                       CURSMOKE = rep(cursmokeMean, 7),
#                       DIABETES = 1)
# ggsurvplot(survfit(coxModelM, newdata = newData), data = cleanDataM, 
#            censor = FALSE, conf.int = TRUE)
# survProbDFM$diabetesYes <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]
# 
# # CURSMOKE no/yes
# # No
# newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                       SYSBP = rep(sysbpMean, 7),
#                       CURSMOKE = 0,
#                       DIABETES = rep(diabetesMean, 7))
# ggsurvplot(survfit(coxModelM, newdata = newData), data = cleanDataM, 
#            censor = FALSE, conf.int = TRUE)
# survProbDFM$cursmokeNo <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]
# 
# # Yes
# newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                       SYSBP = rep(sysbpMean, 7),
#                       CURSMOKE = 1,
#                       DIABETES = rep(diabetesMean, 7))
# ggsurvplot(survfit(coxModelM, newdata = newData), data = cleanDataM, 
#            censor = FALSE, conf.int = TRUE)
# survProbDFM$cursmokeYes <- tail(survfit(coxModelM, newdata = newData)$surv)[5, ]

# # Old code with all covariates of interest, may be useful later
# # With Smoking
# smoking <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                       SYSBP = rep(sysbpMean, 7),
#                       BPMEDS = rep(bpmedsMean, 7),
#                       DIABETES = rep(diabetesMean, 7),
#                       CURSMOKE = rep(1, 7),
#                       ANYCHD = rep(anychdMean, 7))
# ggsurvplot(survfit(coxModelOne, newdata=smoking), 
#            data=cleanData[cleanData$SEX == 1, ], censor=F, conf.int = T)
# tail(survfit(coxModelOne, newdata=newData)$surv)
# 
# # With untreated hypertension
# untreatedHyper <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                              SYSBP = rep(sysbpMean, 7),
#                              BPMEDS = rep(0, 7),
#                              DIABETES = rep(diabetesMean, 7),
#                              CURSMOKE = rep(cursmokeMean, 7),
#                              ANYCHD = rep(anychdMean, 7))
# ggsurvplot(survfit(coxModelOne, newdata=untreatedHyper), 
#            data=cleanData[cleanData$SEX == 1, ], censor=F, conf.int = T)
# tail(survfit(coxModelOne, newdata=newData)$surv)
# 
# # With treated hypertension
# treatedHyper <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                            SYSBP = rep(sysbpMean, 7),
#                            BPMEDS = rep(1, 7),
#                            DIABETES = rep(diabetesMean, 7),
#                            CURSMOKE = rep(cursmokeMean, 7),
#                            ANYCHD = rep(anychdMean, 7))
# ggsurvplot(survfit(coxModelOne, newdata=treatedHyper), 
#            data=cleanData[cleanData$SEX == 1, ], censor=F, conf.int = T)
# tail(survfit(coxModelOne, newdata=newData)$surv)
# 
# # Smoker with diabetes
# smokingAndDiabetes <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
#                                  SYSBP = rep(sysbpMean, 7),
#                                  BPMEDS = rep(bpmedsMean, 7),
#                                  DIABETES = rep(1, 7),
#                                  CURSMOKE = rep(1, 7),
#                                  ANYCHD = rep(anychdMean, 7))
# ggsurvplot(survfit(coxModelOne, newdata=smokingAndDiabetes), 
#            data=cleanData[cleanData$SEX == 1, ], censor=F, conf.int = T)
# tail(survfit(coxModelOne, newdata=newData)$surv)