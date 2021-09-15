# Load libraries
library(lme4)
library(dplyr)
library(ggplot2)

# Aim 1  ------------------------------------------------------------------------
# Read data
withDateTimes <- read_rds("DataProcessed/withDateTimes")

# Booklet times versus MEM Times
(aimOnePlot2 <- ggplot(data = withDateTimes, 
                         aes(x = bookletTime, y = diffMemBooklet, 
                                  color = Collection.Sample)) +
                geom_point() +
                labs(title = "MEM Time Relative to Booklet Time",
                     x = "Booklet Recorded Time",
                     y = "Difference between MEM and Booklet Records "))

# Aim 2  ------------------------------------------------------------------------
# Read data
withDateTimes <- read_rds("DataProcessed/withDateTimes")

aimTwoPlotData <- withDateTimes %>%
        filter(.data$Collection.Sample == 2 | .data$Collection.Sample == 4) %>%
        mutate(book = 
                       ifelse(.data$Collection.Sample == 2,
                              .data$Booklet..Sample.interval.Decimal.Time..mins. - 30,
                              .data$Booklet..Sample.interval.Decimal.Time..mins. - 600),
               mem = 
                       ifelse(.data$Collection.Sample == 2,
                              .data$MEMs..Sample.interval.Decimal.Time..mins. - 30,
                              .data$MEMs..Sample.interval.Decimal.Time..mins. - 600)) %>% 
        pivot_longer(cols = book:mem, names_to = "recordType", 
                     values_to = "adherence")


# Plot book versus MEM adherence
(aimTwoPlot <- ggplot(data = aimTwoPlotData, aes(x = recordType, y = adherence)) +
        geom_boxplot() +
                labs(title = "Protocol Adherence by Record Type",
                     x = "Record Type",
                     y = "Difference Between Protocol and Record"))

# Aim 3 ------------------------------------------------------------------------
# Read data
questionThreeData <- read_rds("DataProcessed/questionThreeData")

# Plot data
(ggplot(data = questionThreeData, aes(x = bookletTime, y = cortisol,
                                      group = interaction(subjectID, day))) +
        geom_point() +
        geom_line() +
        geom_vline(xintercept = 30, color = "red", alpha = .3))

(ggplot(data = questionThreeData, aes(x = bookletTime, y = dhea,
                                      group = interaction(subjectID, day))) +
        geom_point() +
        geom_line() +
        geom_vline(xintercept = 30, color = "red", alpha = .3))
