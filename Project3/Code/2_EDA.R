source("Code/1_ProcessData.R")

library(table1)

# Create Table 1 Data
tableOneData <- dta %>%
    mutate(across(.cols = c(BPMEDS, DIABETES, CURSMOKE, CVD),
                  function (x) { factor(x, levels = c(0, 1),
                                        labels = c("No", "Yes"))})) %>%
    mutate(stroke10yr = factor(.data$stroke10yr, levels = c(0, 1), 
                               labels = c("Censored", "Stroke"))) %>%
    mutate(SEX = factor(.data$SEX, levels = c(1, 2), 
                               labels = c("Male", "Female")))

# Add appropriate labels
label(tableOneData$AGE) <- "Age (years)"
label(tableOneData$SYSBP) <- "SBP (mmHg)"
label(tableOneData$BPMEDS) <- "Use of anti-hypertensive medication"
label(tableOneData$DIABETES) <- "Diabetes"
label(tableOneData$CURSMOKE) <- "Smoking status"
label(tableOneData$TOTCHOL) <- "Total cholesterol (mg/dL)"
label(tableOneData$CVD) <- "History of CVD"
label(tableOneData$timestrk10yr) <- "Time to censoring or stroke (days)"

# Create Table 1
tableOne <- table1(~ timestrk10yr + AGE + BMI + SYSBP + BPMEDS + TOTCHOL + 
                       DIABETES + CURSMOKE + CVD | 
                       tableOneData$SEX * tableOneData$stroke10yr, 
       data = tableOneData, 
       overall = FALSE, 
       render.categorical="FREQ (PCTnoNA%)",
       caption = "Data summary")
