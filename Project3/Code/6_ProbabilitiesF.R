source("Code/4_ModelSelection.R")

# Calculate average covariate values
sysbpMean <- mean(cleanDataF$SYSBP)
bpmedsMean <- mean(cleanDataF$BPMEDS)
cursmokeMean <- mean(cleanDataF$CURSMOKE)
cholMean <- mean(cleanDataF$TOTCHOL)
bmiMean <- mean(cleanDataF$BMI)
diabetesMean <- mean(cleanDataF$DIABETES)
cvdMean <- mean(cleanDataF$DIABETES)

# Calculate continuous covariate means
sysbpQuants <- quantile(cleanDataF$SYSBP)
diabetesQuants <- quantile(cleanDataF$DIABETES)
bmiQuants <- quantile(cleanDataF$BMI)
totcholQuants <- quantile(cleanDataF$TOTCHOL)
cvdMean <- mean(cleanDataF$CVD)

# Create data frame to store survival probabilities
survProbDFF <- data.frame(
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

survProbPlotsF <- list()

# All averages
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = rep(cursmokeMean, 7),
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = rep(cvdMean, 7))
survProbPlotsF[[1]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                  data = cleanDataF, censor = FALSE,
                                  conf.int = FALSE)
survProbDFF$average <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]

# Current Smoker
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = rep(cvdMean, 7))
survProbPlotsF[[2]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                  data = cleanDataF, censor = FALSE,
                                  conf.int = FALSE)
survProbDFF$cursmokeYes <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]

# BPMEDS Yes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = rep(cursmokeMean, 7),
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = 1,
                      CVD = rep(cvdMean, 7))
survProbPlotsF[[3]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                  data = cleanDataF, censor = FALSE,
                                  conf.int = FALSE)
survProbDFF$bpmedsYes <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]

# BPMEDS No
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = rep(cursmokeMean, 7),
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = 0,
                      CVD = rep(cvdMean, 7))
survProbPlotsF[[4]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                  data = cleanDataF, censor = FALSE,
                                  conf.int = FALSE)
survProbDFF$bpmedsNo <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]

# CVD Yes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = rep(cursmokeMean, 7),
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = 1)
survProbPlotsF[[5]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                  data = cleanDataF, censor = FALSE,
                                  conf.int = FALSE)
survProbDFF$cvdYes <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]

# DIABETES Yes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = rep(cursmokeMean, 7),
                      DIABETES = 1,
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = rep(cvdMean, 7))
survProbPlotsF[[6]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                  data = cleanDataF, censor = FALSE,
                                  conf.int = FALSE)
survProbDFF$diabetesYes <- tail(survfit(coxModelF, newdata = newData)$surv)[5, ]

# Smoking and Diabetes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = 1,
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = rep(cvdMean, 7))
survProbPlotsF[[7]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                  data = cleanDataF, censor = FALSE,
                                  conf.int = FALSE)
survProbDFF$smokeYesDiabYes <- tail(survfit(coxModelF, 
                                            newdata = newData)$surv)[5, ]

# Smoking and CVD
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = rep(bpmedsMean, 7),
                      CVD = 1)
survProbPlotsF[[8]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                  data = cleanDataF, censor = FALSE,
                                  conf.int = FALSE)
survProbDFF$smokeYesCVDYes <- tail(survfit(coxModelF, 
                                           newdata = newData)$surv)[5, ]

# Smoking and bpmeds yes
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = 1,
                      CVD = rep(cvdMean, 7))
survProbPlotsF[[9]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                  data = cleanDataF, censor = FALSE,
                                  conf.int = FALSE)
survProbDFF$smokeYesBpmedsYes <- tail(survfit(coxModelF, 
                                              newdata = newData)$surv)[5, ]

# Smoking and bpmeds no
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = rep(diabetesMean, 7),
                      BPMEDS = 0,
                      CVD = rep(cvdMean, 7))
survProbPlotsF[[10]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                   data = cleanDataF, censor = FALSE,
                                   conf.int = FALSE)
survProbDFF$smokeYesBpmedsNo <- tail(survfit(coxModelF, 
                                             newdata = newData)$surv)[5, ]

# All conditions
newData <- data.frame(AGE = c(55, 60, 65, 70, 75, 80, 85),
                      SYSBP = rep(sysbpMean, 7),
                      CURSMOKE = 1,
                      DIABETES = 1,
                      BPMEDS = 1,
                      CVD = 1)
survProbPlotsF[[11]] <- ggsurvplot(survfit(coxModelF, newdata = newData),
                                   data = cleanDataF, censor = FALSE,
                                   conf.int = FALSE)
survProbDFF$allConditions <- tail(survfit(coxModelF, 
                                          newdata = newData)$surv)[5, ]

# Prepare data for plots
survProbDF <- bind_rows(survProbDFM, survProbDFF) %>%
    t() %>%
    as.data.frame() %>%
    janitor::row_to_names(row_number = 1)

colnames(survProbDF) <- paste0(paste0("AGE", rep(colnames(survProbDF, 2))),
                               c(rep("M", 7), rep("F", 7)))

survProbDF <- survProbDF %>%
    mutate_all(function(x) { round((1 - x), 2) })

rownames(survProbDF) <- c("Average", "With smoking", 
                          "With treated hyptertension",
                          "With untreated hypertension", "With diabetes", 
                          "With CVD",
                          "With smoking and diabetes", "With smoking and CVD",
                          "With smoking and treated hypertension", 
                          "With smoking and untreated hypertension",
                          "With all conditions")
