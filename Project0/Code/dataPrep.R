# Load required libraries
library(tidyverse)
library(stringr)

# Load raw data
dataRaw <- read.csv(paste0("/Users/maxmcgrath/Documents/CUDenver/Fall21/BIOS66",
                           "24/bios6624-MaxMcGrath/Project0/DataRaw/Project0_C",
                           "lean_v2.csv"))

# Convert columns to datetimes, then take difference between bookletTime and
#   and memTime
withDateTimes <- dataRaw %>%
    # Convert dates / times to POSIXct
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

# Convert columns to datetimes, then take difference between bookletTime and
#   and memTime. Note however, that unlike above dates are not included in 
#   datetime, so the calculated date will be taken from your system date. This 
#   is so that all differences may be evaluated as if they occurred on the same 
#   day
withDateTimes24 <- dataRaw %>%
    # Convert dates / times to POSIXct
    mutate(bookletTime = as.POSIXct(strptime(.data$Booket..Clock.Time, 
                                             format = "%H:%M")),
           memTime = as.POSIXct(strptime(.data$MEMs..Clock.Time, 
                                         format = "%H:%M")),
           reportedWake = as.POSIXct(strptime(.data$Sleep.Diary.reported.wake.time, 
               format = "%H:%M")),
           diffMemBooklet = difftime(memTime, bookletTime, units = "mins"))

cor(as.numeric(withDateTimes24$bookletTime), as.numeric(withDateTimes24$memTime),
    use = "complete.obs")

ggplot(data = withDateTimes24, mapping = aes(x = bookletTime, y = memTime)) +
    geom_point() +
    geom_smooth(method = 'lm')

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

# Plot book versus MEM adherence
ggplot(data = questionTwoData, aes(x = recordType, y = adherence)) +
    geom_boxplot()

# Create table comparing Book and MEM Adherence
adherenceTable <- questionTwoData %>%
    group_by(recordType) %>%
    summarise(naCount = sum(is.na(.data$adherence)),
              percentLess7_5 = sum(abs(.data$adherence) <= 7.5, na.rm = TRUE) / 
                  sum(!is.na(.data$adherence)), 
              percent7_5to15 = sum(abs(.data$adherence) > 7.5 & 
                                       abs(.data$adherence) <= 15, na.rm = TRUE) / 
                  sum(!is.na(.data$adherence)),
              percentGreater15 = sum(abs(.data$adherence) > 15, na.rm = TRUE) / 
                  sum(!is.na(.data$adherence)),
              percentLess15 = sum(abs(.data$adherence) <= 15, na.rm = TRUE) / 
                  sum(!is.na(.data$adherence)))

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

