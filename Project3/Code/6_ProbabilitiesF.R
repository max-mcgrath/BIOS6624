source("Code/4_ModelSelection.R")

# Calculate average covariate values
sysbpMean <- mean(cleanDataF$SYSBP)
bpmedsMean <- mean(cleanDataF$BPMEDS)
cursmokeMean <- mean(cleanDataF$CURSMOKE)
cholMean <- mean(cleanDataF$TOTCHOL)
bmiMean <- mean(cleanDataF$BMI)
diabetesMean <- mean(cleanDataF$DIABETES)

# Calculate continuous covariate means
sysbpQuants <- quantile(cleanDataF$SYSBP)
diabetesQuants <- quantile(cleanDataF$DIABETES)
bmiQuants <- quantile(cleanDataF$BMI)
totcholQuants <- quantile(cleanDataF$TOTCHOL)
cvdMean <- mean(cleanDataF$CVD)

# Create data frame to store survival probabilities
survProbDFF <- data.frame(
    age = c(55, 60, 65, 70, 75, 80, 85),
    means = rep(NA, 7),
    sysbpQuant1 = rep(NA, 7),
    sysbpQuant2 = rep(NA, 7),
    sysbpQuant3 = rep(NA, 7),
    sysbpQuant4 = rep(NA, 7)
)

# All averages
meanNewData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                          SYSBP = rep(sysbpMean, 7))
ggsurvplot(survfit(coxModelF, newdata = meanNewData), data = cleanDataF, 
           censor = FALSE, conf.int = TRUE)
survProbDFF$means <- tail(survfit(coxModelF, newdata = meanNewData)$surv)[5, ]

# SYSBP at each quantile
# Quantile 1
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpQuants[1], 7))
ggsurvplot(survfit(coxModelF, newdata = newData), data = cleanDataF, 
           censor = FALSE, conf.int = TRUE)
survProbDFF$sysbpQuant1 <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]

# Quantile 2
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpQuants[2], 7))
ggsurvplot(survfit(coxModelF, newdata = newData), data = cleanDataF, 
           censor = FALSE, conf.int = TRUE)
survProbDFF$sysbpQuant2 <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]

# Quantile 3
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpQuants[3], 7))
ggsurvplot(survfit(coxModelF, newdata = newData), data = cleanDataF, 
           censor = FALSE, conf.int = TRUE)
survProbDFF$sysbpQuant3 <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]

# Quantile 4
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpQuants[4], 7))
ggsurvplot(survfit(coxModelF, newdata = newData), data = cleanDataF, 
           censor = FALSE, conf.int = TRUE)
survProbDFF$sysbpQuant4 <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]