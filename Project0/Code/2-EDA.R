# Load libraries
library(lme4)
library(dplyr)
library(ggplot2)

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
