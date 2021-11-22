library(tidyr)
library(dplyr)

# Load data
rawData <- read.csv("DataRaw/frmgham2.csv")

# Filter individuals who previously had stroke, create censorship/event
#   indicator and event time column. 
cleanData <- rawData %>% 
    filter(!(.data$PREVSTRK == 1 & .data$PERIOD == 1)) %>%
    mutate(timestroke10yr = ifelse(.data$TIMESTRK>=3650, 3650, .data$TIMESTRK)) %>%
    mutate(stroke10yr = ifelse(.data$timestroke10yr<3650 & .data$STROKE==1, 1, 0)) %>%
    mutate(timestroke10yr = pmin(.data$TIMESTRK, 3650)) %>%
    filter(.data$PERIOD == 1) %>%
    select(-HDLC, -LDLC) %>%
    drop_na()  %>%
    # Reorder columns, only keep those which are needed
    select(RANDID, SEX, TIME, PERIOD, TIMESTRK, stroke10yr, timestroke10yr,
           AGE, SYSBP, BPMEDS, DIABETES, CURSMOKE, ANYCHD)

# Check sample size values by M/F and event/censored
nEventM <- length(cleanData$stroke10yr[cleanData$SEX == 1 & 
                                           cleanData$stroke10yr == 1])
nEventF <- length(cleanData$stroke10yr[cleanData$SEX == 2 & 
                                           cleanData$stroke10yr == 1])
nCensoredM <- length(cleanData$stroke10yr[cleanData$SEX == 1 & 
                                           cleanData$stroke10yr == 0])
nCensoredF <- length(cleanData$stroke10yr[cleanData$SEX == 2 & 
                                           cleanData$stroke10yr == 0])

