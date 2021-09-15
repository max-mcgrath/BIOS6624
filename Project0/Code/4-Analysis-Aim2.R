# Load libraries
library(lme4)
library(dplyr)

# Aim 2 ------------------------------------------------------------------------
# Read data
questionTwoData <- read_rds("DataProcessed/questionTwoData")

# Create table comparing Book and MEM Adherence
adherenceProportions <- questionTwoData %>% 
    group_by(Collection.Sample) %>%
    summarise(total = n(),
              memNACount = sum(is.na(.data$memAdherence)),
              bookNACount = sum(is.na(.data$bookAdherence)),
              averageMemDiscr = mean(memAdherence, na.rm = TRUE),
              averageBookDiscr = mean(bookAdherence, na.rm = TRUE),
              memPercLess7_5 = sum(abs(.data$memAdherence) <= 7.5, na.rm = TRUE) / 
                  sum(!is.na(.data$memAdherence)),
              bookPercLess7_5 = sum(abs(.data$bookAdherence) <= 7.5, na.rm = TRUE) / 
                  sum(!is.na(.data$bookAdherence)),
              memPercLess15 = sum(abs(.data$memAdherence) <= 15, na.rm = TRUE) / 
                  sum(!is.na(.data$memAdherence)),
              bookPercLess15 = sum(abs(.data$bookAdherence) <= 15, na.rm = TRUE) / 
                  sum(!is.na(.data$bookAdherence)))

# Print data
adherenceProportions
