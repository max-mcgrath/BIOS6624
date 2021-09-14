# Load libraries
library(lme4)
library(dplyr)

# Aim 1 ------------------------------------------------------------------------
# Read data
withDateTimes24 <- read_rds("DataProcessed/withDateTimes24")

# Plot Data
ggplot(data = withDateTimes24, mapping = aes(x = bookletTime, y = memTime)) +
    geom_point() +
    geom_smooth(method = 'lm')

# Create linear model to evaluate correlation
timesLinearModel <- lm(bookletTime ~ memTime, data = withDateTimes24)

# Evaluate linear model assumptions
car::residualPlot(timesLinearModel)

# Summary statistics for time differences between booklet and MEM