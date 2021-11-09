library(tidyr)
library(dplyr)

# Load data
rawData <- read.csv("DataRaw/frmgham2.csv")

# Filter individuals who previously had stroke, create censorship/event
#   indicator and event time column. 
cleanData <- rawData %>%
    filter(!(.data$PERIOD == 1 & .data$PREVSTRK == 1)) %>%
    mutate(timestroke10yr = pmin(.data$TIMESTRK, .data$TIMEDTH, 3650), 
           stroke10yr = ifelse(timestroke10yr < 3650, 1, 0)) %>%
    # Reorder columns, only keep those which are needed
    select(RANDID, SEX, TIME, PERIOD, TIMESTRK, stroke10yr, timestroke10yr) %>% 
    filter(.data$PERIOD == 1)


