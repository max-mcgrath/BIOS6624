# Load libraries
library(lme4)
library(dplyr)
library(car)

# Aim 1 ------------------------------------------------------------------------
# Read data
withDateTimes24 <- read_rds("DataProcessed/withDateTimes24")

# Plot Data
ggplot(data = withDateTimes24, mapping = aes(x = bookletTime, y = memTime)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    labs(title = "Electronic Monitoring Cap vs. Booklet Time",
         x = "Booklet Time", y = "Electronic Monitoring Cap Time")

# Create linear model to evaluate correlation
#   Note: times are converted to numeric as summary.lm() doesn't handle 
#   datetimes well
timesLinearModel <- lm(as.numeric(bookletTime) ~ as.numeric(memTime), 
                       data = withDateTimes24)

# Evaluate linear model assumptions
car::residualPlot(timesLinearModel)

# Summarize findings
summary(timesLinearModel)

# Section below checks possibility of using minutes since waking rather than
#   raw date time
# Read Data
adherenceData <- read_rds("DataProcessed/adherenceData")

# Create linear model to evaluate adherence
adherenceLM <- lm(bookletTime ~ memTime, data = adherenceData)

# Evaluate linear model assumptions (note the correlated errors)
car::residualPlot(adherenceLM)
