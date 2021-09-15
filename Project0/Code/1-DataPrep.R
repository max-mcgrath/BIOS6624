# Load required libraries
library(tidyverse)
library(stringr)
library(hms)

# Load raw data
dataRaw <- read.csv(paste0("/Users/maxmcgrath/Documents/CUDenver/Fall21/BIOS66",
                           "24/bios6624-MaxMcGrath/Project0/DataRaw/Project0_C",
                           "lean_v2.csv"))

# Convert columns to datetimes, then take difference between bookletTime and
#   and memTime
withDateTimes <- dataRaw %>%
    # Convert dates / times to POSIXct
    mutate(bookletTime = as_hms(strptime(.data$Booket..Clock.Time, 
                                             format = "%H:%M")),
           memTime = as_hms(strptime(.data$MEMs..Clock.Time, 
                                         format = "%H:%M")),
           reportedWake = as_hms(strptime(.data$Sleep.Diary.reported.wake.time, 
               format = "%H:%M")),
           diffMemBooklet = difftime(memTime, bookletTime, units = "mins"))

write_rds(withDateTimes24, "DataProcessed/withDateTimes")

adherenceData <- dataRaw %>%
    select(subjectID = SubjectID, 
           day = DAYNUMB,
           bookletTime = Booklet..Sample.interval.Decimal.Time..mins.,
           memTime = MEMs..Sample.interval.Decimal.Time..mins.,
           collectionSample = Collection.Sample) %>%
    filter(!is.na(bookletTime),
           !is.na(memTime))

write_rds(adherenceData, "DataProcessed/adherenceData")

# Question 2  ------------------------------------------------------------------
# Limit data to 2nd (30 minutes after waking) and 4th (10 hours after waking) 
#   observation, calculate respective adherence to study procedure, remove
#   unneeded columns
questionTwoData <- withDateTimes %>%
    filter(.data$Collection.Sample == 2 | .data$Collection.Sample == 4) %>%
    mutate(book = 
               ifelse(.data$Collection.Sample == 2,
                      .data$Booklet..Sample.interval.Decimal.Time..mins. - 30,
                      .data$Booklet..Sample.interval.Decimal.Time..mins. - 600),
           mem = 
               ifelse(.data$Collection.Sample == 2,
                      .data$MEMs..Sample.interval.Decimal.Time..mins. - 30,
                      .data$MEMs..Sample.interval.Decimal.Time..mins. - 600)) %>% 
    pivot_longer(cols = book:mem, names_to = "recordType", values_to = "adherence")

write_rds(questionTwoData, "DataProcessed/questionTwoData")

# Question 3 -------------------------------------------------------------------
# Select only relevant columns, remove any days that have missing or
#   erroneous measurements (still unsure of what constitutes erroneous or how
#   to handle missing data, but this will do for now). Currently using 
#   booklet time for analysis, probably will incorporate memTime later
questionThreeData <- dataRaw %>%
    select(subjectID = SubjectID, 
           day = DAYNUMB,
           bookletTime = Booklet..Sample.interval.Decimal.Time..mins.,
           cortisol = Cortisol..nmol.L., 
           dhea = DHEA..nmol.L.,
           collectionSample = Collection.Sample) %>%
    filter(!is.na(bookletTime),
           (cortisol != 9999),
           (dhea != 1500),
           (cortisol < 40)) %>%
    mutate(logTime = log(bookletTime + 1),
           logCortisol = log(cortisol),
           logDHEA = log(dhea))

# Write data to relevant folder
write_rds(questionThreeData, "DataProcessed/questionThreeData")

