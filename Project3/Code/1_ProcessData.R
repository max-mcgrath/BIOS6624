library(tidyr)
library(dplyr)

# Load data
rawData <- read.csv("DataRaw/frmgham2.csv")

# Filter individuals who previously had stroke, create censorship/event
#   indicator and event time column. 
cleanData <- rawData %>%
    mutate(timestroke10yr = pmin(.data$TIMESTRK, 3650), 
           stroke10yr = ifelse(timestroke10yr < 3650, 1, 0)) %>%
    filter(.data$PERIOD == 1) %>%
    filter(!(.data$PREVSTRK == 1)) %>%
    # Reorder columns, only keep those which are needed
    select(RANDID, SEX, TIME, PERIOD, TIMESTRK, stroke10yr, timestroke10yr,
           AGE, SYSBP, BPMEDS, DIABETES, CURSMOKE, ANYCHD) %>%
    drop_na()

# Check sample size values by M/F and event/censored
nEventM <- length(cleanData$stroke10yr[cleanData$SEX == 1 & 
                                           cleanData$stroke10yr == 1])
nEventF <- length(cleanData$stroke10yr[cleanData$SEX == 2 & 
                                           cleanData$stroke10yr == 1])
nCensoredM <- length(cleanData$stroke10yr[cleanData$SEX == 1 & 
                                           cleanData$stroke10yr == 0])
nCensoredF <- length(cleanData$stroke10yr[cleanData$SEX == 2 & 
                                           cleanData$stroke10yr == 0])
