source("Code/3_ModelSelectionM.R")

# Calculate average covariate values
sysbpMean <- mean(cleanDataM$SYSBP)
bpmedsMean <- mean(cleanDataM$BPMEDS)
diabetesMean <- mean(cleanDataM$DIABETES)
cursmokeMean <- mean(cleanDataM$CURSMOKE)
anychdMean <- mean(cleanDataM$ANYCHD)
cvdMean <- mean(cleanDataM$CVD)
cholMean <- mean(cleanDataM$TOTCHOL)

# All averages
meanNewData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                          SYSBP = rep(sysbpMean, 7),
                          BPMEDS = rep(bpmedsMean, 7),
                          DIABETES = rep(diabetesMean, 7),
                          CURSMOKE = rep(cursmokeMean, 7),
                          ANYCHD = rep(anychdMean, 7),
                          CVD = rep(cvdMean, 7),
                          TOTCHOL = rep(cholMean, 7))
ggsurvplot(survfit(coxModelOne, newdata = meanNewData), data = cleanDataM, 
           censor = FALSE, conf.int = TRUE)
tail(survfit(coxModelOne, newdata = meanNewData)$surv)

# With Smoking
smoking <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      BPMEDS = rep(bpmedsMean, 7),
                      DIABETES = rep(diabetesMean, 7),
                      CURSMOKE = rep(1, 7),
                      ANYCHD = rep(anychdMean, 7))
ggsurvplot(survfit(coxModelOne, newdata=smoking), 
           data=cleanData[cleanData$SEX == 1, ], censor=F, conf.int = T)
tail(survfit(coxModelOne, newdata=newData)$surv)

# With untreated hypertension
untreatedHyper <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                             SYSBP = rep(sysbpMean, 7),
                             BPMEDS = rep(0, 7),
                             DIABETES = rep(diabetesMean, 7),
                             CURSMOKE = rep(cursmokeMean, 7),
                             ANYCHD = rep(anychdMean, 7))
ggsurvplot(survfit(coxModelOne, newdata=untreatedHyper), 
           data=cleanData[cleanData$SEX == 1, ], censor=F, conf.int = T)
tail(survfit(coxModelOne, newdata=newData)$surv)

# With treated hypertension
treatedHyper <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                           SYSBP = rep(sysbpMean, 7),
                           BPMEDS = rep(1, 7),
                           DIABETES = rep(diabetesMean, 7),
                           CURSMOKE = rep(cursmokeMean, 7),
                           ANYCHD = rep(anychdMean, 7))
ggsurvplot(survfit(coxModelOne, newdata=treatedHyper), 
           data=cleanData[cleanData$SEX == 1, ], censor=F, conf.int = T)
tail(survfit(coxModelOne, newdata=newData)$surv)

# Smoker with diabetes
smokingAndDiabetes <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                                 SYSBP = rep(sysbpMean, 7),
                                 BPMEDS = rep(bpmedsMean, 7),
                                 DIABETES = rep(1, 7),
                                 CURSMOKE = rep(1, 7),
                                 ANYCHD = rep(anychdMean, 7))
ggsurvplot(survfit(coxModelOne, newdata=smokingAndDiabetes), 
           data=cleanData[cleanData$SEX == 1, ], censor=F, conf.int = T)
tail(survfit(coxModelOne, newdata=newData)$surv)