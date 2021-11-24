library(survival)

# Read in raw data
dta <- read.csv("DataRaw/frmgham2.csv") 

# Remove people who had a stroke prior to study entry
remove <- dta$RANDID[which(dta$PREVSTRK==1 & dta$PERIOD==1)] 
dta <- subset(dta, !(dta$RANDID %in% remove))

# Create CVD variable
dta$CVD <- ifelse(dta$PREVAP==1 | dta$PREVCHD==1 | dta$PREVMI==1, 1, 0)

# Remove incomplete observations, limit observations to period 1 and 2, only
#   keep observations that were used in analysis
dta <- subset(dta, is.na(dta$BPMEDS)==F & is.na(dta$TOTCHOL)==F & 
                  is.na(dta$BMI)==F)
dta <- subset(dta, dta$PERIOD==1 | dta$PERIOD==2)

# Create a new time variable where times > 10 years are set as exactly 10 years
dta$timestrk10yr <- ifelse(dta$TIMESTRK>=3650, 3650, dta$TIMESTRK) 

# Create an indicator variable for event (0=censored, 1=stroke)
dta$stroke10yr <- ifelse(dta$timestrk10yr<3650 & dta$STROKE==1, 1, 0) 

# For subjects where time of death < time of stroke, use the smaller value 
#   for event time
dta$timestrk10yr <- pmin(dta$TIMEDTH, dta$timestrk10yr)

# Get Period 1 values
diffTableDataP1 <- dta %>%
    select(RANDID, PERIOD, SEX, TOTCHOL, SYSBP, CURSMOKE, BMI, DIABETES,
           BPMEDS, CVD, stroke10yr) %>%
    filter(PERIOD == 1) %>%
    select(-PERIOD)

# Get Period 2 values
diffTableDataP2 <- dta %>%
    select(RANDID, PERIOD, TOTCHOL_P2 = TOTCHOL, SYSBP_P2 = SYSBP, 
           CURSMOKE_P2 = CURSMOKE, BMI_P2 = BMI,
           DIABETES_P2 = DIABETES, BPMEDS_P2 = BPMEDS, CVD_P2 = CVD) %>%
    filter(PERIOD == 2) %>%
    select(-PERIOD)

# Join P1 and P2 (only keeping those which are present for both), create
#   relevant "change" variables
diffTableData <- inner_join(diffTableDataP1, diffTableDataP2, by = "RANDID") %>%
    mutate(startedSmoking = as.numeric(CURSMOKE == 0 & CURSMOKE_P2 == 1),
           stoppedSmoking = as.numeric(CURSMOKE == 1 & CURSMOKE_P2 == 0),
           changedSmoking = as.numeric((CURSMOKE == 1 & CURSMOKE_P2 == 0) |
                                           (CURSMOKE == 0 & CURSMOKE_P2 == 1)),
           becameDiabetic = as.numeric(DIABETES == 0 & DIABETES_P2 == 1),
           startedBPMeds = as.numeric(BPMEDS == 0 & BPMEDS_P2 == 1),
           stoppedBPMeds = as.numeric(BPMEDS == 1 & BPMEDS_P2 == 0),
           changeBPMeds = as.numeric((BPMEDS == 0 & BPMEDS_P2 == 1) | 
                                         (BPMEDS == 1 & BPMEDS_P2 == 0)),
           becameCVD = as.numeric(CVD == 0 & CVD_P2 == 1),
           changeSYSBP = SYSBP_P2 - SYSBP,
           changeBMI = BMI_P2 - BMI,
           changeTOTCHOL = TOTCHOL_P2 - TOTCHOL, .keep = "unused")

# Convert factors
diffTableData <- diffTableData %>%
    mutate(startedSmoking = factor(startedSmoking, levels = c(0, 1),
                                   labels = c("No", "Yes")),
           stoppedSmoking = factor(stoppedSmoking, levels = c(0, 1),
                                   labels = c("No", "Yes")),
           changedSmoking = factor(changedSmoking, levels = c(0, 1),
                                   labels = c("No", "Yes")),
           becameDiabetic = factor(becameDiabetic, levels = c(0, 1),
                                   labels = c("No", "Yes")),
           startedBPMeds = factor(startedBPMeds, levels = c(0, 1),
                                   labels = c("No", "Yes")),
           stoppedBPMeds = factor(stoppedBPMeds, levels = c(0, 1),
                                   labels = c("No", "Yes")),
           changeBPMeds = factor(changeBPMeds, levels = c(0, 1),
                                   labels = c("No", "Yes")),
           becameCVD = factor(becameCVD, levels = c(0, 1),
                                   labels = c("No", "Yes"))) %>%
    mutate(stroke10yr = factor(.data$stroke10yr, levels = c(0, 1), 
                               labels = c("Censored", "Stroke"))) %>%
    mutate(SEX = factor(.data$SEX, levels = c(1, 2), 
                        labels = c("Male", "Female")))

# Add appropriate labels
label(diffTableData$startedSmoking) <- "Started Smoking"
label(diffTableData$stoppedSmoking) <- "Stopped Smoking"
label(diffTableData$changedSmoking) <- "Change in Smoking Status"
label(diffTableData$becameDiabetic) <- "Recognized Diabetic"
label(diffTableData$startedBPMeds) <- "Started BP Medication"
label(diffTableData$stoppedBPMeds) <- "Stopped BP Medication"
label(diffTableData$changeBPMeds) <- "Change in BP Medication Use"
label(diffTableData$becameCVD) <- "Change in history of CVD"
label(diffTableData$changeSYSBP) <- "Change in SBP"
label(diffTableData$changeBMI) <- "Change in BMI"
label(diffTableData$changeTOTCHOL) <- "Change in Cholesterol"

# Create Table of differences
diffTable <- table1(~ startedSmoking + stoppedSmoking + changedSmoking + 
                        becameDiabetic + startedBPMeds + stoppedBPMeds + 
                        changeBPMeds + becameCVD + changeSYSBP + changeBMI +
                        changeTOTCHOL |
                        diffTableData$SEX * diffTableData$stroke10yr, 
                    data = diffTableData, overall = FALSE,
                    render.categorical="FREQ (PCTnoNA%)",
                    caption = "Change in risk factors from first observation period to second")

# Test proportional hazards assumption using Schoenfeld Test
phObjectM <- cox.zph(coxModelM)
ggcoxzph(phObjectM, se = FALSE)

phObjectF <- cox.zph(coxModelF)
ggcoxzph(phObjectF, se = FALSE)

