# Load required libraries
library(tidyverse)
library(stringr)

# Load raw data
dataRaw <- read.csv(paste0("/Users/maxmcgrath/Documents/CUDenver/Fall21/BIOS66",
                           "24/bios6624-MaxMcGrath/Project0/DataRaw/Project0_C",
                           "lean_v2.csv"))

colnames(dataRaw)

# Convert columns to datetimes, then take difference between bookletTime and
#   and memTime
withDateTimes <- dataRaw %>%
    mutate(bookletTime = as.POSIXct(strptime(paste0(.data$Collection.Date, "-", 
                                         .data$Booket..Clock.Time), 
                                  format = "%m/%d/%Y-%H:%M")),
           memTime = as.POSIXct(strptime(paste0(.data$Collection.Date, "-", 
                                     .data$MEMs..Clock.Time), 
                              format = "%m/%d/%Y-%H:%M")),
           reportedWake = as.POSIXct(strptime(
               paste0(.data$Collection.Date, "-",
                      .data$Sleep.Diary.reported.wake.time), 
                                              format = "%m/%d/%Y-%H:%M")),
           diffMemBooklet = difftime(memTime, bookletTime, units = "mins"))

# Plot diff vs bookletTime for EDA
ggplot(data = withDateTimes, aes(x = bookletTime, y = diffMemBooklet, 
                                 color = Collection.Sample)) +
    geom_point() +
    geom_hline(yintercept = 7.5, color = "red", alpha = .3) +
    geom_hline(yintercept = -7.5, color = "red", alpha = .3) +
    geom_hline(yintercept = 15, color = "red", alpha = .3) +
    geom_hline(yintercept = -15, color = "red", alpha = .3)

# Create table of classes for difference between bookletTime and memTime
diffTimeTable <- withDateTimes %>%
    drop_na(diffMemBooklet) %>%
    summarize(percentLess7_5 = sum(abs(.data$diffMemBooklet) < 7.5) / 
                  length(.data$diffMemBooklet), 
              percent7_5to15 = sum(abs(.data$diffMemBooklet) > 7.5 & 
                                       abs(.data$diffMemBooklet) < 15) / 
                  length(.data$diffMemBooklet),
              percentGreater15 = sum(abs(.data$diffMemBooklet) > 15) / 
                  length(.data$diffMemBooklet))

# Same as above but grouped by collection sample
diffTimeTableGrouped <- withDateTimes %>%
    drop_na(diffMemBooklet) %>%
    group_by(Collection.Sample) %>%
    summarize(percentLess7_5 = sum(abs(.data$diffMemBooklet) <= 7.5) / 
                  length(.data$diffMemBooklet), 
              percent7_5to15 = sum(abs(.data$diffMemBooklet) > 7.5 & 
                                       abs(.data$diffMemBooklet) < 15) / 
                  length(.data$diffMemBooklet),
              percentGreater15 = sum(abs(.data$diffMemBooklet) >= 15) / 
                  length(.data$diffMemBooklet))

